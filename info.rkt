#lang info


(define name "sinbad")
(define version "0.0.1")

(define deps '("csv-reading"
               "sxml"))

(define primary-file "main.rkt")

(define release-notes
  (list '(ul
          (li "Proof-of-concept implementation")
          )))

(define required-core-version "6.9")

(define blurb (list "Automated Structure Inference and Binding of Data"))

;(define scribblings '(("scribblings/racketui.scrbl" (multi-page))))

