;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname euro-forex) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require sinbad)

(define forex
  (sail-to "http://www.ecb.europa.eu/stats/eurofxref/eurofxref.zip?67c3c8747f374aba449f070f4e808308"
           (format "csv") (load)))

(fetch-first-number forex "USD")

; filter out "Date" and extra "_col_..." field names...
(define currencies (filter (λ(s) (= 3 (string-length s)))
                           (field-list forex))) 
currencies




(define forex/hist
  (sail-to "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip?67c3c8747f374aba449f070f4e808308"
           (format "csv") (load)))

(data-length forex/hist)   ; ~4800 

(define all-usd (fetch forex/hist "Date" "USD"))  ; list of ~4000+ exchange rate values for USD

(define all-data (fetch forex/hist))              ; fetch all currencies, all dates
(first all-data)

(rest (assoc "Date" (first all-data)))
(rest (assoc "USD" (first all-data)))




