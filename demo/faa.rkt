;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname faa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require sinbad)

(define faa
  (sail-to (string-append "https://soa.smext.faa.gov/asws/api/airport/status/ATL")
           (format "json")
           (param "format" "application/json")
           ;(cache-timeout 300)  ; refresh every 5 minutes
           (load)))

(define-struct port (name loc condition delay?))

(fetch faa (make-port "Name" "State" "Weather/Weather/Temp" "Delay"))

(manifest faa)
