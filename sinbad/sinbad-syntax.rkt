#lang racket


(require sinbad/data-source)
(require (for-syntax syntax/parse))


(provide sail-to)
(provide (all-from-out sinbad/data-source))


; stx:expr stx:(listof clauses) stx:expr -> (values stx:string stx:(list expr) stx:expr/#f stx:expr/#f stx:expr/#f)
; values:
;   1. the format (string, e.g. "json") or #f
;   2. list of set-option/set-param/cache-timeout/etc expressions
;   3. load expr
;   4. sample expr
;   5. describe expr
; at least 3 or 4 must be #f (they cannot both be present)
; the given top-stx is for error reporting
(define-for-syntax (process-clauses top-stx clauses o)
  (let-values ([(fmt e-lst l-exp s-exp d-exp)
                (syntax-parse clauses
                  [(c cs ...) (process-clauses top-stx #'(cs ...) o)]
                  [_ (values #f #'() #f #f #f)])])
    
    (syntax-parse clauses
      [(((~datum cache-timeout) t:expr) cs ...)
       (values fmt #`((send #,o set-cache-timeout! t) #,@e-lst) l-exp s-exp d-exp)]

      [(((~datum param) n:str v:expr) cs ...)
       (values fmt #`((send #,o set-param! n (format "~a" v)) #,@e-lst) l-exp s-exp d-exp)]

      [(((~datum option) n:str v:expr) cs ...)
       (values fmt #`((send #,o set-option! n (format "~a" v)) #,@e-lst) l-exp s-exp d-exp)]

      [(((~datum manifest)) cs ...)
       (values fmt e-lst l-exp s-exp #`(send #,o display-description))]

      [(((~datum load) (~optional force?:boolean #:defaults ([force? #'#f]))) cs ...)
       (cond [l-exp (raise-syntax-error #f "cannot have multiple (load) clauses" top-stx)]
             [s-exp (raise-syntax-error #f "cannot have both (load) and (sample) clauses" top-stx)]
             [else   (values fmt e-lst   #`(send #,o load! force?)   s-exp d-exp)])]

      [(((~datum sample) n (~optional seed #:defaults ([seed #'#f]))) cs ...)
       (cond [l-exp (raise-syntax-error #f "cannot have both (load) and (sample) clauses" top-stx)]
             [s-exp (raise-syntax-error #f "cannot have multiple (sample) clauses" top-stx)]
             [else (values fmt e-lst   l-exp   #`(send #,o load-sample! n seed)   d-exp)])]

      [(((~datum fresh-sample) n (~optional seed #:defaults ([seed #'#f]))) cs ...)
       (cond [l-exp (raise-syntax-error #f "cannot have both (load) and (sample) clauses" top-stx)]
             [s-exp (raise-syntax-error #f "cannot have multiple (sample) clauses" top-stx)]
             [else (values fmt e-lst   l-exp   #`(send #,o load-fresh-sample! n seed)   d-exp)])]

      [(((~datum format) fmt:str) cs ...) (values #`fmt e-lst l-exp s-exp d-exp)]
    
      [(c cs ...) (raise-syntax-error #f "invalid clause" #'c)]
      
      [_ (values #f #'() #f #f #f)])))


(define-syntax (sail-to stx)
  (syntax-parse stx
    [(sail-to (~or ((~datum spec) spec-url) url:expr) clauses ...)

       (let-values ([(fmt e-lst l-exp s-exp d-exp) (process-clauses stx #'(clauses ...) #'o)])
         (let* ([connect-expr (cond
                                [(and (attribute url) fmt) #`(connect url #:format #,fmt)]
                                [(attribute url) #`(connect url)]
                                [else #`(connect-using spec-url)])]
                [load/samp-lst (if l-exp (list l-exp) (if s-exp (list s-exp) '()))]
                [load/samp+descr-lst (if d-exp
                                         (append load/samp-lst (list d-exp))
                                         load/samp-lst)])
           #`(let ([o #,connect-expr])
               #,@e-lst
               #,@load/samp+descr-lst
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
