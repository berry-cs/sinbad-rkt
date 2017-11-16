#lang racket

(module+ test
  (require rackunit))

(require racket/date)

(provide current-seconds
         current-inexact-milliseconds
         seconds->date
         date->string
         (struct-out date))


(provide string-upcase
         string-downcase
         string-position)

(provide degrees->radians
         gc-dist/km
         gc-dist/miles)

(require net/sendurl)
(provide (rename-out (send-url open-browser-to)))




#|
(define EARTH-RADIUS 6371) ; km

;; gc-dist/km : Number Number Number Number -> Number
;; computes the approximate great circle distance (in kilometers)
;; on the earth surface given latitude and longitude values (in
;; degrees) of two points
;; Based on the formula at:
;;  http://www.gcmap.com/faq/gccalc#gchow

#;
(check-within (gc-dist/km 40.7241765 -74.0506564 40.71649 -74.04105)
              1.177    ; km
              .001)    ; +/- tolerance

(define (gc-dist/km lat-1 lng-1 lat-2 lng-2)
  (* EARTH-RADIUS
     (normalize-angle
      (acos (+
             (* (sin (degrees->radians lat-1)) (sin (degrees->radians lat-2)))
             (* (cos (degrees->radians lat-1)) (cos (degrees->radians lat-2)) (cos (degrees->radians (- lng-2 lng-1)))))))))


;; gc-dist/miles : Number Number Number Number -> Number
;; computes the approximate great circle distance (in miles)
;; on the earth surface given latitude and longitude values (in
;; degrees) of two points

#;
(check-within (gc-dist/miles 40.7241765 -74.0506564 40.71649 -74.04105)
              .731
              .001)

#;
(check-within (gc-dist/miles 37.618806 -122.375416   ; SFO 
                             22.308889 113.914722)   ; -> HKG airports
              6927  ; according to http://www.gcmap.com/mapui?P=SFO-HKG
              20)   ; +/- 20 mile error tolerance

(define (gc-dist/miles lat-1 lng-1 lat-2 lng-2)
  (km->miles (gc-dist/km lat-1 lng-1 lat-2 lng-2)))


(define (normalize-angle d)
  (cond
    [(not (zero? (imag-part d))) (normalize-angle (magnitude d))]
    [(< d 0) (+ d 180)]
    [else d]))
|#


;(require math)
(define earth-radius 6371)
 
(define (gc-dist/km lat1 long1 lat2 long2)
  (define (h a b) (sqr (sin (/ (- b a) 2))))
  (* 2 earth-radius 
     (asin (sqrt (+ (h (degrees->radians lat1) (degrees->radians lat2)) 
                    (* (cos (degrees->radians lat1)) (cos (degrees->radians lat2)) (h (degrees->radians long1) (degrees->radians long2))))))))

(define (gc-dist/miles lat-1 lng-1 lat-2 lng-2)
  (km->miles (gc-dist/km lat-1 lng-1 lat-2 lng-2)))
 
(define (degrees->radians d)
  (* pi (/ d 180)))


(define (km->miles k)
  (* k .621371))






(define (string-position needle haystack [start-pos 0])
  (define ps (regexp-match-positions needle haystack start-pos))
  (and ps (car (first ps))))




(module+ test
  (check-eq? (string-position "bye" "hello") #f)
  (check-eq? (string-position "el" "hello") 1)
  (check-eq? (string-position "el" "helloyellow") 1)
  (check-eq? (string-position "el" "helloyellow" 2) 6)
  (check-eq? (string-position "el" "helloyellow" 6) 6)
  (check-eq? (string-position "el" "helloyellow" 7) #f))

