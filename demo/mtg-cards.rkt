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


;; A Set is (make-set String String [Listof Card]
(define-struct set (code name cards))

;; A Card is (make-card String String [Listof String])
(define-struct card (name type colors))


;; use the full-featured version of fetch* to construct more complex data
(define data*
  (map (lambda (k)
         (fetch* D `(path ,k (,make-set "code"
                                        "name"
                                        ((path "cards"
                                               (,make-card "name"
                                                           "type"
                                                           "colors")))))))
       (field-list D)))

(define (list-wrap d)
  (if (list? d) d (list d)))

;; how many cards total?
(length (apply append (map (compose list-wrap set-cards) data*)))    ; 34,000+ !



; using built-in assoc lists instead of structures
(define data*/dict   
  (map (lambda (k)
         (fetch* D `(path ,k (dict (name "name")
                                   (code "code")
                                   (cards ((path "cards"
                                                 (dict (cardname "name")
                                                       (type "type")
                                                       (colors "colors")))))))))
       (field-list D)))





