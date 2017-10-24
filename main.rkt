#lang racket

(require "sinbad-syntax.rkt")
(require (for-syntax syntax/parse))

(provide (all-from-out "sinbad-syntax.rkt"))

(provide load
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


(define-syntax (load stx)
  (syntax-parse stx
    [(load obj:expr) #'(send obj load!)]
    [(load obj:expr force?:boolean) #'(send obj load! force?)]))



(define-syntax (fetch stx)
  (syntax-parse stx
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:str)
     #`(send obj fetch #:select pos path)]
    
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) path:str paths:str ...)
     #`(let ([result (send obj fetch #:select pos #:apply list path paths ...)])  ; constructs a list of lists
         result)]
    
    [(fetch obj:expr (~optional (~seq #:select pos) #:defaults ([pos #'#f])) (proc:id path:str paths:str ...))
     #`(let ([result (send obj fetch #:select pos #:apply proc path paths ...)])  ; apply an explicit function
         (if (cons? result) (first result) result))]))


(define-syntax (fetch-random stx)
  (syntax-parse stx
    [(fetch-random obj:expr rest ...)
     #'(fetch obj #:select 'random rest ...)]))

(define-syntax (fetch-ith stx)
  (syntax-parse stx
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
             [->number (lambda (s) (if (string? s) (string->number s) s))])
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

