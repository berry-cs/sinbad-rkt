;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname faa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require sinbad)

(define faa
  (sail-to (string-append "http://services.faa.gov/airport/status/ATL")
           (format "xml")
           (param "format" "application/xml")
           ;(cache-timeout 300)  ; refresh every 5 minutes
           (load)))

(define-struct port (name loc condition delay?))

(fetch faa (make-port "Name" "State" "Weather/Weather" "Delay"))

(manifest faa)
