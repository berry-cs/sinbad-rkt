;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname divvy) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require sinbad)

(define divvy-live
  (sail-to "https://feeds.divvybikes.com/stations/stations.json"
           (manifest)
           (load)))

(fetch-random divvy-live "stationName" "availableBikes" "availableDocks" "status"
                (base-path "stationBeanList"))

(fetch-random divvy-live (assoc "stationName" "availableBikes" "availableDocks" "status")
                (base-path "stationBeanList"))

(define live-stns
  (fetch divvy-live "stationName" "availableBikes" "availableDocks" "status"
                (base-path "stationBeanList")))






(define divvy-trips
  (sail-to "https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2017_Q1Q2.zip"
           (format "csv")
           (option "file-entry" "Divvy_Trips_2017_Q1.csv")
           (sample 100)   ; or (load) for all the data
           (manifest)))

(define id (fetch-random divvy-trips "trip_id"))
(define start-time (fetch-random divvy-trips "start_time"))
(define user-type (fetch-random divvy-trips "usertype"))
(define duration (fetch-random divvy-trips "tripduration"))
; note: subsequent 'fetch-random's will fetch from the
;       *same* row of data as the first one (trip_id)

(string-append "Trip " (number->string id)
               " was made by a " user-type
               " at " start-time
               " and lasted for a duration of "
               (number->string (quotient duration 60)) " minutes")






(define divvy-stns
  (sail-to "https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2017_Q1Q2.zip"
           (format "csv")
           (option "file-entry" "Divvy_Stations_2017_Q1Q2.csv")
           (load)
           (manifest)))

(define-struct stn (name cap city))

(fetch divvy-stns (make-stn "name" "dpcapacity" "city"))


