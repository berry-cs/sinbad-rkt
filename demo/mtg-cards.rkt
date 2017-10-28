;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mtg-cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require sinbad)

(define D
  (sail-to "https://mtgjson.com/json/AllSets.json.zip"
           (option "ignore-missing" "colors")
           (load)))

(define data
  (map (lambda (k) 
         (fetch D (assoc "name" "cards/name" "cards/type" "cards/colors") (base-path k)))
       (field-list D)))


(define data*/SCG
  (fetch* D #:base-path "SCG"
          `(dict (name "name")
                 (cards ((path "cards"
                               (dict (cardname "name")
                                     (type "type")
                                     (colors "colors"))))))))


