;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fuel-economy) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require sinbad)


(fetch
 (sail-to "http://www.fueleconomy.gov/ws/rest/fuelprices"
          (format "xml") (load) (manifest)))

(fetch
 (sail-to "http://www.fueleconomy.gov/ws/rest/fuelprices"
          (format "xml") (load))
 "premium")




(define all-vehicles
  (sail-to "http://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip"
           (load) (manifest)))

(define-struct auto (make model year trans city-mpg hwy-mpg))

(fetch-random all-vehicles (make-auto "make" "model" "year" "trany" "city08" "highway08"))
(data-length all-vehicles)




(define makes-data
  (sail-to "http://www.fueleconomy.gov/ws/rest/vehicle/menu/make"
           (format "xml") (param "year" 2012) (load) (manifest)))

(define makes (fetch makes-data "menuItem/value"))





(define model-options
  (sail-to "http://www.fueleconomy.gov/ws/rest/vehicle/menu/options"
           (format "xml")
           (param "year" "2012") (param "make" "Honda") (param "model" "Fit")
           (load) (manifest)))

(fetch model-options (list "text" "value") (base-path "menuItem"))




(fetch
 (sail-to "http://www.fueleconomy.gov/ws/rest/vehicle/31819"
         (format "xml") (load))
 (make-auto "make" "model" "year" "trany" "city08" "highway08"))








