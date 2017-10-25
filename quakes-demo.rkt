#lang racket

(require racket/date
         "main.rkt")

(define-struct quake (title time mag) #:transparent)


;; number -> string
(define (ts->timestr v)
  (date->string (seconds->date (/ v 1000)) #t))

;; quake -> quake
(define (print-quake q)
  (printf "Quake: ~a (~a)~n" (quake-title q) (ts->timestr (quake-time q))))

;; 
(define (main)
  (define ds (sail-to "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson"
                      (cache-timeout 180)   ; 3 minutes
                      ;(manifest)
                      (load)))
  
  (let LOOP ([collected '()])
    (define data (reverse
                  (fetch (load ds)
                         (make-quake "features/properties/title" "features/properties/time" "features/properties/mag"))))
    (define news (for/list ([q data] #:when (not (member (quake-title q) collected)))
                   (print-quake q)
                   (quake-title q)))
    (when (not (empty? news)) (printf "~a~n" (length collected)))
    (if (and (> (length collected) 10) (empty? news))
        (LOOP (map quake-title data))  ; ... so that collected doesn't grow forever
        (LOOP (append collected news)))))


(main)