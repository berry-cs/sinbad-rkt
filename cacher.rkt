#lang racket

(require file/md5
         json
         "dot-printer.rkt"
         "util.rkt")

(module+ test
  (require rackunit))

(provide (struct-out cacher)
         NEVER-CACHE
         NEVER-RELOAD
         sinbad-cache-enabled
         default-cacher
         clear-entire-cache
         lookup-entry-data
         stale?
         update-timeout
         update-directory
         resolve-cache-path)

;; *parameter* to globally enable/disable caching behavior
(define sinbad-cache-enabled (make-parameter #t (lambda (v) (equal? v #t))))

;; constants - public
(define NEVER-CACHE 0)
(define NEVER-RELOAD -1)

;; constants - private
(define DEFAULT-CACHE-DIR     (build-path (find-system-path 'temp-dir) "sinbad_cache"))
(define MINIMUM-TIMEOUT-VALUE 100)   ; milliseconds



(struct cacher (directory expiration))

(define default-cacher (cacher DEFAULT-CACHE-DIR NEVER-RELOAD))

;; cacher string -> cacher
;; Returns an new cacher like c but with the given cache directory
(define (update-directory c new-dir)
  (struct-copy cacher c [directory new-dir]))

;; cacher number -> cacher
;; Returns an new cacher like c but with the given cache expiration value
;; specified in milliseconds.
(define (update-timeout c new-to)
  (define checked-to
    (if (< 0 new-to MINIMUM-TIMEOUT-VALUE)
        (begin
          (fprintf (current-error-port)
                   "Warning: cannot set cache timeout to less than ~a msec.~n" MINIMUM-TIMEOUT-VALUE)
          MINIMUM-TIMEOUT-VALUE)
        new-to))
  (struct-copy cacher c [expiration checked-to]))

(module+ test
  (let ([sp (open-output-string)])
    (parameterize ([current-error-port sp])  ; (open-output-nowhere)
      (check-equal? (cacher-expiration (update-timeout default-cacher 10))
                    MINIMUM-TIMEOUT-VALUE)
      (check-true   (string-prefix? (get-output-string sp) "Warning:"))
      (check-equal? (cacher-expiration (update-timeout default-cacher NEVER-CACHE))
                    NEVER-CACHE)
      (check-equal? (cacher-expiration (update-timeout default-cacher 10000))
                    10000))))


;; cacher string string -> boolean
;; Determine if the given path+subtag is not in the cache or expired.
(define (stale? c path subtag)
  (cond
    [(not (and (sinbad-cache-enabled) (cacheable? c path subtag)))  #t]
    [else
     (define e (cache-entry-for c path subtag))
     (or (false? e)
         (expired? e (cacher-expiration c)))]))


;; cacher string string -> boolean
;; Delete the stored data in the cache directory and remove info from the cache index file
(define (clear-cache-data c tag subtag)
  (define e (cache-entry-for c tag subtag))
  (if (not e)
      #f
      (begin
        (when (entry-data-valid? e)
          (delete-file (entry-data e)))
        (remove-entry c e))))


;; cacher string string -> #f or entry
;; Return the cache entry object for the tag+subtag pair if it exists
(define (cache-entry-for c tag subtag)
  (define ces (read-cache-entry-list (cache-index-file-path c tag)))
  (findf
   (lambda (e) (and (string=? tag (entry-tag e))
                    (string=? subtag (entry-subtag e))))
   ces))


;; cacher string string -> #f or string
(define (lookup-entry-data c tag subtag)
  (define e (cache-entry-for c tag subtag))
  (cond
    [(false? e) #f]
    [(expired? e (cacher-expiration c))
     (clear-cache-data c tag subtag)
     #f]
    [else (entry-data e)]))


;; cacher entry -> boolean
;; Replace the cache entry in the cache index file that has the same 
;; tag+subtag pair as the given cache entry with the given cache entry. 
;; If there is not already an entry for that tag+subtag, then the given
;; entry is added to the list in the index file.
(define (add-or-update-entry c e-new)
  (define cache-idx (cache-index-file-path c (entry-tag e-new)))
  (define ces (cons e-new
                    (remove e-new
                            (read-cache-entry-list cache-idx)
                            entry-tags-match)))
  (write-cache-entry-list cache-idx ces))



;; cacher entry -> boolean
;; Remove a particular entry from the index file that it belongs to.
(define (remove-entry c e-key)
  (define cache-idx (cache-index-file-path c (entry-tag e-key)))
  (define ces (filter
               (lambda (e) (not (entry-tags-match e-key e)))
               (read-cache-entry-list cache-idx)))
  (write-cache-entry-list cache-idx ces))



;; cacher string [input-port or #f] -> (list string [string or #f] [string or #f])
#|      Reads the data (as bytes) from the given path (or already opened file
        object) and stores it in a temporary file in the local cache directory
        (using cache_byte_data). 
        
        Returns a triple of:
         - the path to the cached data file
         - an alternate name for the data (e.g. from content-disposition header)
         - an encoding for the data if detected by raw_create_input.|#
(define (read-and-cache c path [fp #f])
  (define D (box #f))  ; dot-printer
  
  (dynamic-wind
   (lambda ()
     (when (and (not fp) (smells-like-url? path))
       (set-box! D (dot-printer (format "Downloading ~a (this may take a moment)" path)))))
   
   (lambda ()    ; try:
     (match-define
       (list actual-fp local-name encoding)
       (if (not fp)
           (raw-create-input path)
           (list fp #f #f)))
     
     (define byte-data (port->bytes actual-fp))
     (close-input-port actual-fp)

     (define cached-file-path (cache-byte-data c path byte-data))
     (list cached-file-path local-name encoding))
   
   (lambda ()     ; finally:
     (when (unbox D) (stop-dot-printer (unbox D))))))



;; cacher string string -> string(path)
#|      Returns a local path to the cached data for the given path+subtag.
        
        If caching is not enabled, or the path is already a local path, or
        not cacheable for whatever reason, then the path is returned unchanged.
        
        If subtag is 'main', then the resource identified by path will be 
        downloaded and cached if it is not already, or if it has become stale.
        
        If subtag is NOT 'main', but the path+subtag is not in the cache, 
        then #f is returned.|#
(define (resolve-cache-path c path subtag)
  (call/cc
   (λ (return)

     ; first make sure caching is enabled and that the path is not a local file
     (when (or (not (and (sinbad-cache-enabled) (cacheable? c path subtag)))
               ; and that the cache-index-file can be generated/read
               (not (cache-index-file-path c path)))
       (return path))

     (define pre-e (cache-entry-for c path subtag))
     (define e
       (if (and pre-e (not (entry-data-valid? pre-e)))
           (begin (clear-cache-data c path subtag)
                  #f)
           pre-e))

     (define cache-path (and e (entry-data e)))

     (cond
       [(and (string-prefix? subtag "main")
             (or (not cache-path)  ; not previously cached
                                   ; or has expired...
                 (and e (expired? e (cacher-expiration c)))))

        (printf "Refreshing cache for: ~a (~a) ~a~n" path subtag (cacher-expiration c))
        
        (match-define
          (list new-cache-path local-name encoding)
          (read-and-cache c path))

        (when cache-path    ; need to remove the old cached file
          (delete-file cache-path))

        (add-or-update-entry
         c (entry path subtag new-cache-path (current-milliseconds)))

        ; if local-name or enc were detected by raw_create_input via the read_and_cache function
        ; then cache those as well
        (when local-name
          (printf "adding real-name ~a for ~a~n" local-name path)
          (add-or-update-entry
           c (entry path "real-name" local-name (current-milliseconds))))

        (when encoding
          (printf "adding encoding ~a for ~a~n" encoding path)
          (add-or-update-entry
           c (entry path "encoding" encoding (current-milliseconds))))

        new-cache-path]

       [(and (not (string-prefix? subtag "main"))
             (and e (expired? e (cacher-expiration c))))
        #f]

       [else
        (printf "Using previously cached data for: ~a (~a)~n" path subtag)
        cache-path]))))



;; cacher string string input-port -> boolean
#|      Reads data from the given file object and stores it in the local
        cache directory (using read_and_cache) and also updates the cache
        index file accordingly.
        
        (This is sort of a simple, manual version of resolve_path.)
        
        Returns True if the whole procedure succeeded.|#
(define (add-to-cache c path subtag fp)
  (call/cc
   (λ (return)

     ; first make sure caching is enabled and that the path is not a local file
     (when (or (not (sinbad-cache-enabled))
               (= (cacher-expiration c) NEVER-CACHE)
               (not (cache-index-file-path c path)))
       (return #f))

     (define e (cache-entry-for c path subtag))
     (define cache-path (and e (entry-data e)))

     (printf "Refreshing cache for: ~a (~a)~n" path subtag)

     (match-define
       (list new-cache-path _ _)
       (read-and-cache c (format "~a (~a)" path subtag) fp))
     
     (when cache-path    ; need to remove the old cached file
          (delete-file cache-path))
     
     (add-or-update-entry
         c (entry path subtag new-cache-path (current-milliseconds)))
     #t)))



;; cacher string [bytes or string] [#f or string] -> string
;; Creates a temporary file in the cache directory and writes the 
;; given data to it, encoding it first if the data is a string
;;
;; tag is used to determine the subdirectory in which the temp file is created.
;; Note: a new temporary file is created each time this function is called
;;
;; Returns the path (string) to the newly-created file 
(define (cache-byte-data c tag data [enc #f])
  (define stuff
    (cond [(bytes? data) data]
          [(string? enc) (parameterize ([current-locale enc])
                           (string->bytes/locale data))]
          [else (string->bytes/utf-8 data)]))
  
  (define cache-dir (build-path (cacher-directory c)
                                (cache-subdir-name tag)))
  
  (when (not (directory-exists? cache-dir))
    (make-directory* cache-dir))

  (define temp-path (make-temporary-file "cache~a.dat" #f cache-dir))
  
  (with-output-to-file temp-path
    (lambda () (write-bytes stuff))
     #:exists 'replace)
  
  (path->string temp-path))


;; Delete the entire cache directory being maintained by the given cacher
(define (clear-entire-cache c)
  (when (directory-exists? (cacher-directory c))
    (delete-directory/files (cacher-directory c))))


(define (cacheable? c path subtag)
  ;; if it's something other than "main" data being accessed (e.g. "schema")
  ;;   then it can be in the cache, even if the main is a local file
  ;; otherwise, if it's the "main" data we're considering, then only cache
  ;;  if it smells like a URL and the cacher specific setting is not set 
  ;;  to never cache
  (and (sinbad-cache-enabled)
       (or (not (string-prefix? subtag "main"))
           (and (smells-like-url? path)
                (not (= (cacher-expiration c) NEVER-CACHE))))))

(module+ test
  (check-true (cacheable? default-cacher "http://cs.berry.edu" "main"))
  (check-true (cacheable? default-cacher "cacher.rkt" "schema"))
  
  (check-false (cacheable? default-cacher "cacher.rkt" "main")))


;; cacher  string  ->  path or false
;; gets the cache index file file for the given tag under the given cacher
;; the file must exist (is created and filled with empty list of entries if
;; it doesn't already)
(define (cache-index-file-path c tag)
  (define D (cacher-directory c))
  
  (when (not (directory-exists? D))
    (make-directory* D))
  (cond
    [(not (directory-exists? D))   #f]
    [else
     (define cache-idx (path->complete-path (build-path D (cache-index-name tag))))
     (when (not (file-exists? cache-idx))
       (write-cache-entry-list cache-idx empty))
     (and (file-exists? cache-idx)
          cache-idx)]))


(define (cache-index-name tag)
  (string-append  "idx" (bytes->string/utf-8 (md5 tag)) ".json"))

(define (cache-subdir-name tag)
  (string-append "dat" (bytes->string/utf-8 (md5 tag))))

;; string/path -> (listof entry)
(define (read-cache-entry-list cache-idx-name)
  (with-input-from-file cache-idx-name
    (lambda () (json->entries (read-json)))))

;; string/path (listof entry) -> boolean
(define (write-cache-entry-list cache-idx-name ces)
  (with-output-to-file
      cache-idx-name
    (lambda () (write-json (entries->json ces)))
    #:exists 'replace)
  #t)



(module+ test
  (check-equal? (cache-index-name "http://cs.berry.edu")
                "idx0f1cacfe4398213523f4943524a234de.json")
  (check-equal? (cache-subdir-name "http://cs.berry.edu")
                "dat0f1cacfe4398213523f4943524a234de"))




;;;; ================ cache entry structure ===============================

;; timestamp is in milliseconds
(struct entry (tag subtag data timestamp) #:transparent)



;; jsexpr -> (listof entry)
(define (json->entries jse)
  (define (json->entry e)
    (entry (hash-ref e 'tag)
           (hash-ref e 'subtag)
           (hash-ref e 'cachedata)
           (hash-ref e 'timestamp)))
  
  (match jse
    [(list e ...) (map json->entry e)]
    [_ '()]))


;; (listof entry) -> jsexpr
(define (entries->json es)
  (map (lambda (e)
         (make-hasheq `((tag . ,(entry-tag e))
                        (subtag . ,(entry-subtag e))
                        (cachedata . ,(entry-data e))
                        (timestamp . ,(entry-timestamp e)))))
       es))

(module+ test
  (let ([es (list (entry "aaa" "bb" "blah" 10)
                (entry "ccc" "dd" "bfoolah" 20))])
    (check-equal? (json->entries (entries->json es))
                  es)))



;; entry -> boolean
;; Check to see if the entry's cachedata refers to an actual readable file.
(define (entry-data-valid? e)
  (and (entry-data e)
       (file-exists? (entry-data e))
       (member 'read (file-or-directory-permissions (entry-data e)))
       (member 'write (file-or-directory-permissions (entry-data e)))
       #t))

(module+ test
  (check-true (entry-data-valid? (entry "aaa" "bbb" "cacher.rkt" 10)))
  (check-false (entry-data-valid? (entry "aaa" "bbb" "frooblah" 10))))



;; entry entry -> boolean
;; determine if a pair of tag+subtag's match
(define (entry-tags-match e1 e2)
  (and (string=? (entry-tag e1) (entry-tag e2))
       (string=? (entry-subtag e1) (entry-subtag e2))))

(module+ test
  (check-true (entry-tags-match (entry "aaa" "bbb" "blah" 10)
                                (entry "aaa" "bbb" "bfoolah" 20)))
  (check-false (entry-tags-match (entry "aaa" "bbb" "blah" 10)
                                 (entry "aaa" "bbc" "blah" 10))))


;; entry number[milliseconds] -> boolean
;; Check to see if the given entry has expired (relative to current time).

(define (expired? ent expiration)
  (define diff (- (current-milliseconds)
                  (entry-timestamp ent)))
  (and (>= expiration 0)
       (> diff expiration)))

(module+ test
  (check-true (expired? (entry "aaa" "bbb" "blah" (- (current-milliseconds) 10))
                          5))
  (check-false (expired? (entry "aaa" "bbb" "blah" (- (current-milliseconds) 10))
                          15)))

