#lang racket

(provide sail-to)

(require (for-syntax syntax/parse))

(require "data-source.rkt")


; stx:expr stx:(listof clauses) stx:expr -> (values stx:string stx:(list expr) stx:expr/#f stx:expr/#f)
; values:
;   1. the format (string, e.g. "json") or #f
;   2. list of set-option/set-param/cache-timeout/etc expressions
;   3. load expr
;   4. sample expr
; at least 3 or 4 must be #f (they cannot both be present)
; the given top-stx is for error reporting
(define-for-syntax (process-clauses top-stx clauses o)
  (let-values ([(fmt e-lst l-exp s-exp)
                (syntax-parse clauses
                  [(c cs ...) (process-clauses top-stx #'(cs ...) o)]
                  [_ (values #f #'() #f #f)])])
    
    (syntax-parse clauses
      [(((~datum cache-timeout) t) cs ...)
       (values fmt #`((send #,o set-cache-timeout! t) #,@e-lst) l-exp s-exp)]

      [(((~datum param) n v) cs ...)
       (values fmt #`((send #,o set-param! n v) #,@e-lst) l-exp s-exp)]

      [(((~datum option) n v) cs ...)
       (values fmt #`((send #,o set-option! n v) #,@e-lst) l-exp s-exp)]

      [(((~datum load) (~optional force? #:defaults ([force? #'#f]))) cs ...)
       (cond [l-exp (raise-syntax-error #f "cannot have multiple (load) clauses" top-stx)]
             [s-exp (raise-syntax-error #f "cannot have both (load) and (sample) clauses" top-stx)]
             [else   (values fmt e-lst   #`(send #,o load! force?)   s-exp)])]

      [(((~datum sample) n) cs ...)
       (cond [l-exp (raise-syntax-error #f "cannot have both (load) and (sample) clauses" top-stx)]
             [s-exp (raise-syntax-error #f "cannot have multiple (sample) clauses" top-stx)]
             [else (values fmt e-lst   l-exp   #`(send #,o sample! n))])]

      [(((~datum format) fmt) cs ...) (values #`fmt e-lst l-exp s-exp)]
    
      [(c cs ...) (raise-syntax-error #f "invalid clause" #'c)]
      
      [_ (values #f #'() #f #f)])))


(define-syntax (sail-to stx)
  (syntax-parse stx
    [(sail-to url clauses ...)

       (let-values ([(fmt e-lst l-exp s-exp) (process-clauses stx #'(clauses ...) #'o)])
         (let ([connect-expr (if fmt
                                 #`(connect url #:format #,fmt)
                                 #`(connect url))]
               [load/samp-lst (if l-exp (list l-exp) (if s-exp (list s-exp) '()))])
           #`(let ([o #,connect-expr])
               #,@e-lst
               #,@load/samp-lst
               o)))]))


#|


(define A
  (sail-to "http://services.faa.gov/airport/status/ATL"
           (format "json")
           (param "format" "application/json")
           (option "file-entry" "...")
           (cache-timeout 300)
           (load)
           ))
A


|#
