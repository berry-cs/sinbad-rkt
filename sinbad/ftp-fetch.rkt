#lang racket

(require net/ftp)
(require net/url)

(provide ftp-fetch)


#|
Downloads a file from an FTP URL and saves to a temporary file in the
specified save directory. Returns the file name of the temporary file
created. Note that the name of the temporary file is the downloaded
file's name but prefixed with some random stuff.
|#
(define (ftp-fetch the-url #:save-dir [savedir (find-system-path 'temp-dir)]
                   #:port [the-port 21]
                   #:user [username "anonymous"]
                   #:password [password "sinbad.data@gmail.com"])
  (match (string->url the-url)
    [(url scheme user host port path-abs? path query fragment)
     (cond
       [(not (and (string=? scheme "ftp")
                  (string? host)
                  (cons? path)
                  (empty? query)
                  (false? fragment)))   (error (format "bad ftp url: ~a" url))]
       [else
        (define login-port (or (and (string? port) (string->number port)) the-port))
        (define login-name (or user username))
        (define conn (ftp-establish-connection host login-port login-name password))
        (define the-file (path/param-path (last path)))

        (for [(pp (drop-right path 1))]
          (ftp-cd conn (path/param-path pp)))
        ;(printf "list: ~a~n to fetch: ~a~n" (ftp-directory-list conn ".") the-file)
        (ftp-download-file conn savedir the-file)

        (define renamed-file
          (make-temporary-file (string-append "sinbadftp_~a_" the-file)
                               (build-path savedir the-file)
                               savedir))
        (ftp-close-connection conn)
        renamed-file])]
    [_ (error (format "bad ftp url: ~a" url))]))


(module+ no-test
  (define URL "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/1901/029070-99999-1901.gz")
  (ftp-fetch URL)
  )

#|
(define CONN (ftp-establish-connection "ftp.ncdc.noaa.gov" 21 "anonymous" "sinbad.data@gmail.com"))

(ftp-directory-list CONN "/pub/data/noaa/isd-lite/1901/" )
(ftp-cd CONN "/pub/data/noaa/isd-lite/1901/")
(ftp-download-file CONN "/Users/nhamid/Desktop/" "029070-99999-1901.gz"
                   #:progress (lambda (get-count)
                                (define-values (bytes evt) (get-count))
                                (printf "~a ~a~n" bytes evt)))

(ftp-close-connection CONN)



(match (regexp-match #rx"ftp:\\/\\/([^:\\/\\s]+)(.*\\/)([^\\/\\s]+)" url)
    [(list full/match domain dir file)
     (printf "domain: ~a~ndirectory: ~a~nfile: ~a~n" domain dir file)
     ]
    [_ #f])
|#