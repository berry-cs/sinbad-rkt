#lang racket

(require sinbad/sinbad-syntax
         sinbad/extras)
(require (for-syntax syntax/parse))

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
    
    [(has-fields? obj:expr paths:str ... (~optional ((~datum base-path) bp) #:defaults ([bp #'#f])))
     #`(send obj has-fields? paths ... #:base-path bp)]))



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
    [(fetch* obj:expr rest ...) #'(send obj fetch rest ...)]))


(define-syntax (fetch stx)
  (syntax-parse stx
    [(fetch s:str stuff ...)
     (raise-syntax-error 'fetch "first parameter should be a data-source" #'s)]
    
    [(fetch obj:expr)
     #`(unwrap-if-single (hash->assoc (send obj fetch)))]
    
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:str)
     #`(unwrap-if-single (hash->assoc (send obj fetch #:select pos path)))]
    
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:str paths:str ... (~optional ((~datum base-path) bp) #:defaults ([bp #'#f])))
     #`(let ([result (send obj fetch #:base-path bp #:select pos #:apply list path paths ...)])  ; constructs a list of lists
         (unwrap-if-single (hash->assoc result)))]

    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) ((~literal assoc) path:str paths:str ...) (~optional ((~datum base-path) bp) #:defaults ([bp #'#f])))
     #`(let ([result (send obj fetch #:base-path bp #:select pos #:apply 'dict path paths ...)])  ; build dictionary (assoc list) of data
         (unwrap-if-single (hash->assoc result)))]
    
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) (proc:id path:str paths:str ...) (~optional ((~datum base-path) bp) #:defaults ([bp #'#f])))
     #`(let ([result (send obj fetch #:base-path bp #:select pos #:apply proc path paths ...)])  ; apply an explicit function
         (unwrap-if-single result))]))


(define-syntax (fetch-random stx)
  (syntax-parse stx
    [(fetch-random obj:expr)
     #`(hash->assoc (send obj fetch #:select 'random))]
    [(fetch-random obj:expr rest ...)
     #'(fetch obj #:select 'random rest ...)]))

(define-syntax (fetch-ith stx)
  (syntax-parse stx
    [(fetch-ith obj:expr i:exact-nonnegative-integer)
     #`(hash->assoc (send obj fetch #:select i))]
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
    [(fetch-float obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:str)
     #`(let ([result (send obj fetch #:select pos path)]
             [->number (lambda (s) (if (string? s) (or (string->number (string-trim s)) 0) 0))])
         (if (list? result) (map ->number result) (->number result)))]))

(define-syntax (fetch-first-number stx)
  (syntax-parse stx
    [(fetch-first-number obj:expr path:str) #'(fetch-number obj #:select 0 path)]))

(define-syntax (fetch-ith-number stx)
  (syntax-parse stx
    [(fetch-ith-number obj:expr i:exact-nonnegative-integer path:str)
     #'(fetch-number obj #:select i path)]))

(define-syntax (fetch-random-number stx)
  (syntax-parse stx
    [(fetch-random-number obj:expr path:str) #'(fetch-number obj #:select 'random path)]))


(define-syntax (fetch-boolean stx)
  (syntax-parse stx
    [(fetch-boolean obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:str)
     #`(let ([result (send obj fetch #:select pos path)]
             [->bool (lambda (s)
                       (cond [(string? s) (member (string-downcase s) '("true" "t" "1" "yes" "y"))]
                             [(number? s) (not (zero? s))]
                             [else s]))])
         (if (list? result) (map ->bool result) (->bool result)))]))

(define-syntax (fetch-first-boolean stx)
  (syntax-parse stx
    [(fetch-first-boolean obj:expr path:str) #'(fetch-boolean obj #:select 0 path)]))

(define-syntax (fetch-ith-boolean stx)
  (syntax-parse stx
    [(fetch-ith-boolean obj:expr i:exact-nonnegative-integer path:str)
     #'(fetch-boolean obj #:select i path)]))

(define-syntax (fetch-random-boolean stx)
  (syntax-parse stx
    [(fetch-random-boolean obj:expr path:str) #'(fetch-boolean obj #:select 'random path)]))

