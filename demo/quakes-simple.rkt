;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname quakes-simple) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require sinbad)

(define-struct quake (title time mag))

(define Q
  (sail-to "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson"
           (cache-timeout 180)
           (load)
           (manifest)))

(define (quake-ts->timestr v)
  (date->string (seconds->date (/ v 1000)) #t))



; demonstrates full-featured  fetch*  API
(define data
  (fetch* Q `(path "features" "properties"
                   (,make-quake "place"
                                (,quake-ts->timestr "time")
                                "mag"))))

; ... with keyword arguments
(fetch* Q `(path "properties"
                   (,make-quake "place"
                                (,quake-ts->timestr "time")
                                "mag"))
        #:base-path "features"
        #:select 'random)

data
