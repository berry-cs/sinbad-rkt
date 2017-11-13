#lang racket

(require sinbad/sinbad-syntax
         sinbad/extras)
(require (for-syntax syntax/parse))
(require lang/prim)

(provide (all-from-out sinbad/sinbad-syntax))
(provide (all-from-out sinbad/extras))

(provide load
         manifest
         field-list
         data-length
         has-fields?
         cache-directory
         clear-entire-cache
         export
         fetch*
         fetch
         fetch-ith
         fetch-first
         fetch-second
         fetch-third
         fetch-random
         fetch-number
         fetch-first-number
         fetch-ith-number
         fetch-random-number
         fetch-boolean
         fetch-first-boolean
         fetch-ith-boolean
         fetch-random-boolean)


(define-syntax (clear-entire-cache stx)
  (syntax-parse stx
    [(clear-entire-cache obj:expr) #'(send obj clear-cache)]))


(define-syntax (load stx)
  (syntax-parse stx
    [(load obj:expr) #'(send obj load!)]
    [(load obj:expr force?:boolean) #'(send obj load! force?)]))


(define-syntax (manifest stx)
  (syntax-parse stx
    [(manifest obj:expr) #'(send obj display-description)]))


(define-syntax (field-list stx)
  (syntax-parse stx
    [(field-list obj:expr (~optional base-path #:defaults ([base-path #'#f])))
     #`(let ([data (let unwrap ([d (send obj fetch #:base-path base-path)])
                             (if (cons? d) (unwrap (first d)) d))])
         (if (dict? data) (map symbol->string (dict-keys data)) '()))]))


(define-syntax (data-length stx)
  (syntax-parse stx
    [(data-length obj:expr (~optional base-path #:defaults ([base-path #'#f])))
     #`(let ([data (send obj fetch #:base-path base-path)])
         (if (list? data) (length data) 0))]))


(define-syntax (has-fields? stx)
  (syntax-parse stx
    [(has-fields? obj:expr (~optional ((~datum base-path) bp) #:defaults ([bp #'#f])))
     (raise-syntax-error 'has-fields? "at least one field path must be provided" stx)]

    [(has-fields? obj:expr paths:expr ...((~datum base-path) bp))
     #`(send obj has-fields? paths ... #:base-path bp)]
    
    [(has-fields? obj:expr paths:expr ...)
     #`(send obj has-fields? paths ...)]))



(define-syntax (cache-directory stx)
  (syntax-parse stx
    [(cache-directory obj:expr)
     #`(send obj cache-directory)]))


(define-syntax (export stx)
  (syntax-parse stx
    [(export obj:expr path:str)
     #`(let ([r (send obj export path)])
         #t)]))



(define (hash->assoc data)
  (cond
    [(list? data) (map hash->assoc data)]
    [(hash? data)
     (sort
      (for/list ([(k v) (in-dict data)])
        (cons (format "~a" k) (hash->assoc v)))
      (lambda (p1 p2) (string<=? (car p1) (car p2))))]
    [else data]))

(define (unwrap-if-single data)
  (cond
    [(and (list? data) (= 1 (length data))) (first data)]
    [else data]))


; direct full-featured fetch
(define-syntax (fetch* stx)
  (syntax-parse stx
    [(fetch* obj:expr rest ...) #'(unwrap-if-single (hash->assoc (send obj fetch rest ...)))]))


(define-syntax (fetch stx)
  (syntax-parse stx
    [(fetch s:str stuff ...)
     (raise-syntax-error 'fetch "first parameter should be a data-source" #'s)]
    
    [(fetch obj:expr)
     #`(unwrap-if-single (hash->assoc (send obj fetch)))]
    
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) ((~literal assoc) path:expr paths:expr ...) (~optional ((~datum base-path) bp) #:defaults ([bp #'#f])))
     #`(let ([result (send obj fetch #:base-path bp #:select pos #:apply 'dict path paths ...)])  ; build dictionary (assoc list) of data
         (unwrap-if-single (hash->assoc result)))]
    
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) (proc:id path:expr paths:expr ...) (~optional ((~datum base-path) bp) #:defaults ([bp #'#f])))
     #`(let ([result (send obj fetch #:base-path bp #:select pos #:apply (first-order->higher-order proc) path paths ...)])  ; apply an explicit function
         (unwrap-if-single result))]

    ;; this case comes after the two above because otherwise it is matched first
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:expr)   ; selecting a single path - usually path:str, but could be expr
     #`(unwrap-if-single (hash->assoc (send obj fetch #:select pos path)))]

    ;; this case is duplicated to catch the base-path clause version first, otherwise (base-path ...) matches as a paths:expr
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:expr paths:expr ... ((~datum base-path) bp))
     #`(let ([result (send obj fetch #:base-path bp #:select pos #:apply list path paths ...)])  ; constructs a list of lists
         (unwrap-if-single (hash->assoc result)))]
    
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:expr paths:expr ... )
     #`(let ([result (send obj fetch #:select pos #:apply list path paths ...)])  ; constructs a list of lists
         (unwrap-if-single (hash->assoc result)))]))


(define-syntax (fetch-random stx)
  (syntax-parse stx
    [(fetch-random s:str stuff ...)
     (raise-syntax-error 'fetch-random "first parameter should be a data-source" #'s)]
    [(fetch-random obj:expr)
     #`(unwrap-if-single (hash->assoc (send obj fetch #:select 'random)))]
    [(fetch-random obj:expr rest ...)
     #'(fetch obj #:select 'random rest ...)]))

(define-syntax (fetch-ith stx)
  (syntax-parse stx
    [(fetch-ith s:str stuff ...)
     (raise-syntax-error 'fetch-ith "first parameter should be a data-source" #'s)]
    [(fetch-ith obj:expr i:exact-nonnegative-integer)
     #`(unwrap-if-single (hash->assoc (send obj fetch #:select i)))]
    [(fetch-ith obj:expr i:exact-nonnegative-integer rest ...)
     #'(fetch obj #:select i rest ...)]))

(define-syntax (fetch-first stx)
  (syntax-parse stx [(fetch-first obj:expr rest ...) #'(fetch-ith obj 0 rest ...)]))

(define-syntax (fetch-second stx)
  (syntax-parse stx [(fetch-second obj:expr rest ...) #'(fetch-ith obj 1 rest ...)]))

(define-syntax (fetch-third stx)
  (syntax-parse stx [(fetch-third obj:expr rest ...) #'(fetch-ith obj 2 rest ...)]))



(define-syntax (fetch-number stx)
  (syntax-parse stx
    [(fetch-number s:str stuff ...)
     (raise-syntax-error 'fetch-number "first parameter should be a data-source" #'s)]
    [(fetch-number obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:expr)
     #`(let ([result (send obj fetch #:select pos path)]
             [->number (lambda (s) (cond
                                     [(number? s) s]
                                     [(string? s) (or (string->number (string-trim s)) 0)]
                                     [else 0]))])
         (unwrap-if-single (if (list? result) (map ->number result) (->number result))))]))

(define-syntax (fetch-first-number stx)
  (syntax-parse stx
    [(fetch-first-number obj:expr path:expr) #'(fetch-number obj #:select 0 path)]))

(define-syntax (fetch-ith-number stx)
  (syntax-parse stx
    [(fetch-ith-number obj:expr i:exact-nonnegative-integer path:expr)
     #'(fetch-number obj #:select i path)]))

(define-syntax (fetch-random-number stx)
  (syntax-parse stx
    [(fetch-random-number obj:expr path:expr) #'(fetch-number obj #:select 'random path)]))


(define-syntax (fetch-boolean stx)
  (syntax-parse stx
    [(fetch-boolean s:str stuff ...)
     (raise-syntax-error 'fetch-boolean "first parameter should be a data-source" #'s)]
    [(fetch-boolean obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:expr)
     #`(let ([result (send obj fetch #:select pos path)]
             [->bool (lambda (s)
                       (cond [(string? s) (if (member (string-downcase s) '("true" "t" "1" "yes" "y"))
                                               #t
                                               #f)]
                             [(number? s) (not (zero? s))]
                             [else #f]))])
         (unwrap-if-single (if (list? result) (map ->bool result) (->bool result))))]))

(define-syntax (fetch-first-boolean stx)
  (syntax-parse stx
    [(fetch-first-boolean obj:expr path:expr) #'(fetch-boolean obj #:select 0 path)]))

(define-syntax (fetch-ith-boolean stx)
  (syntax-parse stx
    [(fetch-ith-boolean obj:expr i:exact-nonnegative-integer path:expr)
     #'(fetch-boolean obj #:select i path)]))

(define-syntax (fetch-random-boolean stx)
  (syntax-parse stx
    [(fetch-random-boolean obj:expr path:expr) #'(fetch-boolean obj #:select 'random path)]))





;; Adjust printing in *SL

(define my-ph
  (let ([ph (current-print)])
   (λ (value) (if (is-a? value data-source%) (display value) (ph value)))))

(current-print my-ph)
