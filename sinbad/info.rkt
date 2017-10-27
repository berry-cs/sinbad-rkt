#lang info


(define name "sinbad")
(define version "0.0.1")
(define blurb (list "Automated Structure Inference and Binding of Data"))
(define repositories '("4.x"))
(define primary-file "main.rkt")

(define deps '("base"
               "csv-reading"
               "sxml"
               "srfi-lite-lib"))

(define build-deps         '("racket-doc"
                             "scribble-lib"
                             "rackunit-lib"))

(define required-core-version "6.9")
;(define scribblings '(("scribblings/racketui.scrbl" (multi-page))))

