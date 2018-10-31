#lang racket

(require file/gunzip
         net/url
         "ftp-fetch.rkt")

(provide smells-like-url?
         smells-like-gzip?
         smells-like-zip?
         create-input
         raw-create-input)

(module+ test
  (require rackunit))


;; Determine if the given path seems like a URL
(define (smells-like-url? p)
  ;;     Currently, only things that start off http://
  ;;     https:// or ftp:// are treated as URLs.
  (or (is-http-url? p) (is-ftp-url? p)))

(module+ test
  (check-equal? (is-http-url? "https://www.noaa.gov/hi.txt") true)
  (check-equal? (is-http-url? "http://web.com/hi.txt") true)
  (check-equal? (is-http-url? "https://data.berry.edu/long/path-something/x.csv") true)
  (check-equal? (is-http-url? "ftp://www.noaa.gov/hi.txt") false)
  (check-equal? (is-http-url? "file:data.berry.edu/long/path-something/x.csv") false)
  
  (check-equal? (is-ftp-url? "https://www.noaa.gov/hi.txt") false)
  (check-equal? (is-ftp-url? "http://web.com/hi.txt") false)
  (check-equal? (is-ftp-url? "https://data.berry.edu/long/path-something/x.csv") false)
  (check-equal? (is-ftp-url? "ftp://www.noaa.gov/hi.txt") true)
  (check-equal? (is-ftp-url? "file:data.berry.edu/long/path-something/x.csv") false))

(define (is-http-url? p)
  (and (regexp-match url-regexp p)
       (member (url-scheme (string->url p)) '("http" "https"))
       true))

(define (is-ftp-url? p)
  (and (regexp-match url-regexp p)
       (member (url-scheme (string->url p)) '("ftp"))
       true))


(define (smells-like-gzip? p)
  (and (string? p)
       (string-contains? p ".gz")))

(define (smells-like-zip? p)
  (and (string? p)
       (string-contains? p ".zip")))




(define (create-input path)
  (first (raw-create-input path)))


#|
    Returns a triple, ( fp, real_name, enc ), containing a file-type object
    for the given path, an alternate name for the resource, and an encoding.
    
    If the path is a normal file, produces a file-object for that file.
    
    If the path is a URL, makes a request to that URL and
    returns an input stream to read the response.
    
    In the process of processing the response from the URL,
    if a name for the file is found (e.g. in 
    a Content-Disposition header), that is produced as real_name. 
    Otherwise real_name is returned as #f.
    
    If an encoding is determined from the URL response
    headers, it is included, otherwise the third element 
    of the triple is #f.
|#
(define (raw-create-input path)
  (define charset (box #f))
  (define local-name (box #f))

  (define (location-header hdrs)
    (define re #px"^[Ll]ocation:\\s+(.*)$")
    (second (regexp-match re
                          (findf (lambda (line) (regexp-match re line)) hdrs))))
  
  (define (get-port+headers/follow-redirects path)
    (define-values (ip hdrs) (get-pure-port/headers (string->url path)
                                                       (list "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36")
                                                       #:status? #t))
    (define hdr-lines (string-split hdrs #px"[\r\n]"))
    (define code (string->number (second (regexp-match #px"^\\S+ (\\d+) " (first hdr-lines)))))
    (cond
      [(member code '(301 302 307 308))
       (get-port+headers/follow-redirects (location-header hdr-lines))]
      [else (values ip hdrs)]))
  
  (define fp
    (cond
      [(or (not path)
           (not (string? path))) #f]
      [(is-ftp-url? path)
       (define local-file-path (ftp-fetch path))
       (open-input-file local-file-path)]
      [(is-http-url? path)
       (define-values (ip hdrs) (get-port+headers/follow-redirects path))
       (for ([h (string-split hdrs #px"[\r\n]")])
         (define R1 (regexp-match #px"^[Cc]ontent-[Dd]isposition.*filename=\"?([^\\s\"]*)\"?" h))
         (define R2 (regexp-match #px"^Content-Type:.*charset=\"?([^\\s\"]*)\"?" h))
         (when R1 (set-box! local-name (second R1)))
         (when R2 (set-box! charset (second R2))))
       ip]
    [(string-prefix? path "wss:")  #f]
    [else (open-input-file (expand-user-path path))]))

  (define final-fp
    (cond
      [(or (smells-like-gzip? path)
            (and (string? (unbox local-name))
                 (smells-like-gzip? (unbox local-name))))

       (define ubp (open-output-bytes))
       (gunzip-through-ports fp ubp)
       (define unzipped-bytes-port (open-input-bytes (get-output-bytes ubp)))
       (close-input-port fp)   ; is this safe here?
       unzipped-bytes-port]
      [else fp]))
  

  (list final-fp (unbox local-name) (unbox charset)))



#;
(module+ test
  ; content disposition header...
  (check-equal? (second
                 (raw-create-input "http://api.worldbank.org/v2/en/country/per?downloadformat=csv"))
                 "API_PER_DS2_en_csv_v2.zip")
  ; only one file in the zip...
  (check-equal? (second
                 (raw-create-input "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=xml"))
                 "API_SP.POP.TOTL_DS2_en_xml_v2.zip")
  ; gzipped input...
  (check-equal? (read (create-input "https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/StormEvents_details-ftp_v1.0_d1950_c20170120.csv.gz"))
                'BEGIN_YEARMONTH))

