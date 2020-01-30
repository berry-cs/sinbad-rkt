#lang racket

(require rackunit)
(require sinbad)
(define ds (sail-to "./test-kiva.json"
                    ;(manifest)
                    (load)))


(check-equal?
 (send ds fetch "data/lend/loans/totalCount")
 809)

(check-equal?
 (send ds fetch #:base-path "data/lend/loans/values" "name")
 '("Mona" "Raquel" "Reginah" "Irene's Group" "Agnes"))

(check-equal?
 (send ds fetch #:base-path "data/lend/loans/values" #:select 0 "name")
 "Mona")


;; using full `sig` API:

; R-path -> R-proc-list-2 -> R-proc-dict -> R-label-dict
(check-equal?
 (send ds fetch #:base-path "data/lend/loans/values" `(,string-length "name"))
 '(4 6 7 13 5))

; R-path -> R-label-dict
(check-equal?
 (send ds fetch `(path "data" "lend" "loans" "totalCount"))
 809)

(struct person/1 (name country count) #:transparent)

;  R-path -> R-proc-list-1 -> R-proc-any
(check-equal?
 (send ds fetch
       #:base-path "data/lend/loans/values"
       #:select 2
       `(,person/1 "name" (path "geocode" "country" "name") (value 42)))
 (person/1 "Reginah" "Kenya" 42))

(struct loan/2 (sector amt) #:transparent)
(struct person/2 (name country info) #:transparent)

(check-equal?
 (send ds fetch
       #:select 0
       `(path "data" "lend" "loans" "values"
              (,person/2 "name"
                         (path "geocode" "country" "name")
                         (,loan/2 (path "activity" "name")
                                  "loanAmount"))))
 (person/2 "Mona"
              "United States"
              (loan/2 "Restaurant" "10000.00")))

; select, 'dict sig
(check-match
 (send ds fetch
       #:select 0
       `(path  "data" "lend" "loans" "values"
               (dict ("name" "name") ("country" (path "geocode" "country" "name")))))
 (hash-table (name "Mona") (country "United States")))


; test flatten in R-proc-any
(check-equal?
 (send ds fetch `(,+ (path "data" "lend" "loans" "values" (,string-length "name"))))
 35)

(check-equal?
 (send ds fetch `(,list (path "data" "lend" "loans" "totalCount")
                       (,+ (path "data" "lend" "loans" "values" (,string-length "name")))))
 (list 809 35))

; R-proc-any
(check-equal?
 (send ds fetch `(path "data" "lend" "loans" "totalCount" (,- (value 9))))
 -9)

; R-proc-list-2   very simple
(check-equal?
 (send (sail-to "test-num-list.json" (load))
       fetch `(,sqr "."))
 `(25 36 49))

; R-proc-any   only a string
(check-equal?
 (send (sail-to "test-string.json" (load)) fetch `(,string-length "."))
 5)

; R-proc-any   only a string
(check-equal?
 (send (sail-to "test-dict.json" (load)) fetch `(,string-length "country"))
 7)

(check-equal?
 (send (sail-to "test-dict.json" (load))
       fetch `(,string-append "name" "country" (,number->string "age")))
 "MonaEcuador7")

; with nested path
(check-equal?
 (send (sail-to "test-dict.json" (load))
      fetch `(,string-append "name" "country" (,number->string "age") (path "address" "street")))
 "MonaEcuador7main st")

; R-dict-list-2 --> R-dict-any*  with list
(check-match
 (send (sail-to "test-num-list.json" (load))
       fetch `(dict ("a" ".")))
 (list (hash-table (a 5)) (hash-table (a 6)) (hash-table (a 7))))

; R-dict-any   with single string
(check-match
 (send (sail-to "test-string.json" (load))
       fetch `(dict ("a" ".")))
 (hash-table (a "hello")))

; R-label-list -> R-label-dict
(check-match
 (send (sail-to "test-dict-list.json" (load))
       fetch "name")
 '("Mona" "Jim" "Alice"))

; R-label-list -> R-path -> R-label-dict
(check-match
 (send (sail-to "test-dict-list.json" (load))
       fetch `(path "address" "street"))
 '("main st" "broad st" "center ave"))

