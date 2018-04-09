;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname quick-start) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require sinbad)

(define ds-kiva
  (sail-to "http://api.kivaws.org/v1/loans/newest.json"))
(load ds-kiva)
(manifest ds-kiva)


(define kiva-ds
  (sail-to "http://api.kivaws.org/v1/loans/newest.json"
           (load)
           (manifest)))


(fetch-first kiva-ds "loans/name" "loans/use" "loans/location/country" "loans/loan_amount")

(fetch-first kiva-ds "name" "use" "location/country" "loan_amount" (base-path "loans"))

;(fetch ds-kiva "loans/name" "loans/use" "loans/location/country" "loans/loan_amount")
(fetch kiva-ds "name" "use" "location/country" "loan_amount" (base-path "loans"))


(fetch-random kiva-ds "loans/name")
(fetch-random kiva-ds "loans/use")
(fetch-random kiva-ds "loans/loan_amount")

(fetch-random kiva-ds "loans/name" "loans/use" "loans/loan_amount")

(load kiva-ds)
(fetch-random kiva-ds "loans/name")
(fetch-random (load kiva-ds) "loans/use")
(fetch-random (load kiva-ds) "loans/loan_amount")

(fetch-random kiva-ds "loans/name" "loans/use" "loans/loan_amount")

(define all-kiva (fetch kiva-ds))   ; produces assoc-list

(length (rest (assoc "loans" all-kiva)))
(first (rest (assoc "loans" all-kiva)))

"OK"

(fetch-random kiva-ds "loans")
(fetch-ith kiva-ds 5 "loans")

"
exploring
"

(field-list kiva-ds)
(field-list kiva-ds "loans")

(data-length kiva-ds)
(data-length kiva-ds "loans")


#;
(define kiva-ds-fresh
  (sail-to "http://api.kivaws.org/v1/loans/newest.json"
           (cache-timeout 300)
           (load)))
#;
(fetch-first kiva-ds-fresh "loans")


(cache-directory kiva-ds)
;(clear-entire-cache kiva-ds)


"query params ----------------------------------------------------------------"

(define kiva-ds/p
  (sail-to "http://api.kivaws.org/v1/loans/newest.json"
           (cache-timeout 300)
           (param "page" 5)
           (param "per_page" 7)
           (load)))

(data-length kiva-ds/p "loans")
(fetch kiva-ds/p "paging/page")

"data format ----------------------------------------------------------------"


(define kiva-ds/xml
  (sail-to "http://api.kivaws.org/v1/loans/newest.xml"
           (load)
           (manifest)))

(fetch-first kiva-ds/xml "name" "use" "location/country" "loan_amount" (base-path "loans/loan"))

#;
(define ks/json
  (sail-to "https://s3.amazonaws.com/weruns/forfun/Kickstarter/Kickstarter_2015-10-22T09_57_48_703Z.json.gz"
           (load)
           (manifest)))
#;
(data-length ks/json "data/projects")


;;; memory limit:   4096

#;
(sail-to "https://s3.amazonaws.com/weruns/forfun/Kickstarter/Kickstarter_2015-10-22T09_57_48_703Z.zip"
         (load))
; could not infer data format for https://s3.amazonaws.com/weruns/forfun/Kickstarter/Kickstarter_2015-10-22T09_57_48_703Z.zip

#;
(sail-to "https://s3.amazonaws.com/weruns/forfun/Kickstarter/Kickstarter_2015-10-22T09_57_48_703Z.zip"
         (load)
         (format "csv"))
; failed to load data: Specify a file-entry from the ZIP file: (Kickstarter010.csv Kickstarter012.csv Kickstarter002.csv Kickstarter006.csv Kickstarter003.csv Kickstarter011.csv Kickstarter013.csv Kickstarter004.csv Kickstarter008.csv Kickstarter009.csv Kickstarter.csv Kickstarter007.csv Kickstarter005.csv Kickstarter001.csv)


(define ks/zip
  (sail-to "https://s3.amazonaws.com/weruns/forfun/Kickstarter/Kickstarter_2015-10-22T09_57_48_703Z.zip"
           (load)
           (format "csv")
           (option "file-entry" "Kickstarter003.csv")
           (manifest)))

(string-length (first (fetch ks/zip "projects")))   ; --- huge!


"SAMPLING ----------------------------------------------------------------------"

#;
(define ks/samp
  (sail-to "https://s3.amazonaws.com/weruns/forfun/Kickstarter/Kickstarter_2015-10-22T09_57_48_703Z.json.gz"
           (sample 100 42)
           ;(fresh-sample 100 42)
           (manifest)))
#;
(fetch-first ks/samp "data/projects/name")


"OPTION SETTINGS ----------------------------------------------------------------------"


(define per-ds
  (sail-to "http://api.worldbank.org/v2/en/country/per"
           (format "csv")
           (param "downloadformat" "csv")
           (option "skip-rows" "4")
           (option "file-entry" "API_PER_DS2_en_csv_v2.csv")
           (load)
           (manifest)))

(define per-ds/header
  (sail-to "http://api.worldbank.org/v2/en/country/per"
           (format "csv")
           (param "downloadformat" "csv")
           (option "skip-rows" "5")
           (option "header" "Country,CCode,Indicator,ICode,year60,year61,year62,year63,year64,year65,year66,year67,year68,year69,year70,year71,year72,year73,year74,year75,year76,year77,year78,year79,year80,year81,year82,year83,year84,year85,year86,year87,year88,year89,year90,year91,year92,year93,year94,year95,year96,year97,year98,year99,year00,year01,year02,year03,year04,year05,year06,year07,year08,year09,year10,year11,year12,year13,year14,year15,year16")
           (option "file-entry" "API_PER_DS2_en_csv_v2.csv")
           (load)
           (manifest)))

"SPEC FILES ----------------------------------------------------------------------"

(define per-ds/spec
  (sail-to (spec "https://raw.githubusercontent.com/berry-cs/sinbad/master/docs/peru_wb.spec")
           (manifest)))

(load per-ds/spec)
(manifest per-ds/spec)

"EXPORT SPEC ----------------------------------------------------------------------"

#;
(export kiva-ds/p "~/Desktop/spec.txt")
; only works if file doesn't already exist



"QUERY vs PATH ----------------------------------------------------------------------"

#;
(sail-to (spec "https://raw.githubusercontent.com/berry-cs/sinbad/master/docs/faa_status.spec")
         (load))
; not ready to load; missing params: airport_code

(sail-to (spec "https://raw.githubusercontent.com/berry-cs/sinbad/master/docs/faa_status.spec")
         (manifest))

(sail-to (spec "https://raw.githubusercontent.com/berry-cs/sinbad/master/docs/faa_status.spec")
         (load)
         (manifest)
         (param "airport_code" "ATL"))

; note the URL filled in with path param




"---------------------------------------- BINDING TO STRUCTURES "

(define-struct loan (person use amt ctry))

(fetch-random kiva-ds (make-loan "name" "use" "loan_amount" "location/country") (base-path "loans"))




