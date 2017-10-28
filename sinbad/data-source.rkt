#lang racket

;; THIS CODE IS PROOF-OF-CONCEPT AND NEEDS TO BE RE-ORGANIZED AND DOCUMENTED PROPERLY


(require net/uri-codec
         file/unzip
         racket/random
         (only-in json jsexpr->bytes jsexpr->string))
(require sinbad/cacher
         sinbad/dot-printer
         sinbad/plugin
         sinbad/util)

(module+ test
  (require rackunit))



(provide data-source%
         (struct-out exn:fail:sinbad)
         connect
         connect-using
         NEVER-CACHE
         NEVER-RELOAD
         dot-printer-enabled)


(define *debug* #f)

(define-syntax dprintf
  (syntax-rules ()
    [(dprintf blah ...) (when *debug* (printf blah ...))]))



(define *predefined-plugins*
  (list (hasheq 'name "JSON (built-in)" 'type-ext "json" 
                'data-infer (json-infer) 'data-factory json-access)
        (hasheq 'name "XML (ssax built-in)" 'type-ext "xml"
                'data-infer (xml-infer) 'data-factory xml-access)
        (hasheq 'name "CSV (built-in)" 'type-ext "csv"
                'data-infer (csv-infer #f) 'data-factory csv-access)))


(define (connect path #:format [data-format #f])
  (define type-ext (and data-format (string-downcase data-format)))

  (or
   
   (for/first ([p *predefined-plugins*]
               #:when (or (and type-ext (string=? type-ext (dict-ref p 'type-ext)))
                          (and (not type-ext) (matched-by? (dict-ref p 'data-infer) path))))
     (new data-source% (name path) (path path) (type-ext (dict-ref p 'type-ext)) (plugin p)))
   
   (error
    (if type-ext
        (format "no data source plugin for type ~a" type-ext)
        (format "could not infer data format for ~a" path)))))


(define (connect-using spec-path)
  (define the-cust (make-custodian))
  (dynamic-wind
   (lambda () (void))
   
   (lambda ()    ; try:
     (parameterize ([current-custodian the-cust])
       (load-spec-file (create-input spec-path))))

   (lambda ()    ; finally:
     (custodian-shutdown-all the-cust))))


; key is string
; type is either 'path or 'query
; description is string
(struct param (key type description required?))

(define (make-param key type [desc #f] [req? #f])
  (param key type desc req?))

(define (param->jsexpr prm val)
  (let ([e (hasheq 'key (param-key prm)
                   'type (symbol->string (param-type prm))
                   'required (param-required? prm))])
    (let ([e (if (param-description prm)
                 (hash-set e 'description (param-description prm))
                 e)])
      (let ([e (if val (hash-set e 'value val) e)])
        e))))


(struct exn:fail:sinbad exn:fail ()
  #:extra-constructor-name make-exn:fail:sinbad
  #:transparent)

(define-syntax sinbad-error
  (syntax-rules ()
    [(sinbad-error msg) (raise (make-exn:fail:sinbad msg (current-continuation-marks)))]))



(define data-source%
  (class* object% (printable<%>)
    (super-new)
    ;(inspect #f)
    
    (init-field name
                path
                [(format-type type-ext)])
    (init plugin)
    
    (field
     [info-url    #f]
     [info-text   #f]
     [random-index #f])

    ;; private fields ----------------------------------------------------------
    (define cacher       default-cacher)
    (define connected?   (and path #t))
    (define load-ready?  #f)
    (define loaded?      #f)

    (define data-infer (dict-ref plugin 'data-infer))
    (define data-factory ((dict-ref plugin 'data-factory)))

    (define the-data #f)
    (define sampled?     #f)   ; whether the-data is a sample of the actual data

    (define ignore-error-fields '())   ; list of fields for which to not report warning messages
    (define invalid-fields-reported (make-parameter '()))  ; used to control printout of warning messages
    
    ; all the following hash tables assume *symbols* as the key
    
    (define option-settings (hasheq))  ; generic options for the data-source; the data-factory
    ; also maintains its own set of options

    ; params are connection-related parameters,
    ; either in a query string or filling in part of the path
    (define params (hasheq))   ; keeps track of *all* parameters information available for this data source
    (define param-values (hasheq)) ; # keeps track of the values of the supplied parameters
    


    ;; printing ----------------------------------------------------------------

    (define/public (custom-print out quote-depth)
      (fprintf out "<data-source-object: ~a>" path))
    
    (define/public (custom-write out)
      (fprintf out "<data-source-object: ~a>" path))

    (define/public (custom-display out)
      (fprintf out "<data-source-object: ~a>" path))


    ;; methods -----------------------------------------------------------------

    (define/public (has-data?)
      (and connected? loaded?))

    
    (define/public (get-full-path-url)
      (when (not (ready-to-load?))
        (sinbad-error "Cannot finalize path: not ready to load"))

      (define param-keys (sort (dict-keys param-values)  (λ (s1 s2) (string<=? (symbol->string s1)
                                                                               (symbol->string s2)))))
      
      ; sort so that the URL doesn't differ, forcing a cache refresh unnecessarily because of reordering of dictionary keys
      (define query-params
        (for/list ([k param-keys]
                   #:when (let ([prm (dict-ref params k #f)])
                            (or (not prm) (eq? 'query (param-type prm)))))
          (define v (dict-ref param-values k))
          (cons k v)))

      (define full-path-non-subst
        (if (= 0 (dict-count query-params))
            path
            (string-append path "?" (alist->form-urlencoded query-params))))

      (define full-path-subst
        (for/fold ([cur-path full-path-non-subst])
                  ([k param-keys]
                   #:when (let ([prm (dict-ref params k #f)])
                            (and prm (eq? 'path (param-type prm)))))
          (define k-str (symbol->string k))
          (define v (dict-ref param-values k))
          (string-replace cur-path (string-append "@{" k-str "}") v)))
      
      full-path-subst)

    
    (define/private (ready-to-load?)
      (set! load-ready? (or load-ready? (= 0 (length (missing-params)))))
      (and connected? load-ready?))

    
    (define/private (missing-params)
      (filter (λ (k)
               (and (param-required? (dict-ref params k))
                    (not (dict-ref param-values k #f))))
             (dict-keys params)))


    (define/public (load! [force-reload? #f])
      (when (not connected?) (sinbad-error (format "not connected: ~a" path)))
      (when (not (ready-to-load?))
        (sinbad-error (format "not ready to load; missing params: ~a"
                              (string-join (map symbol->string (missing-params)) ", "))))

      (define subtag "main")
      (define full-path (get-full-path-url))
      (define stale-data? (stale? cacher full-path subtag))

      (cond
        [(and loaded? (not (or stale-data? force-reload? sampled?)))
         (set! random-index #f)
         this]
        [else
         ; TODO - prep usage info to share ....
         
         (define D (box #f))
         (define the-cust (make-custodian))
         
         (dynamic-wind
          (lambda ()
            (set-box! D (dot-printer (format "Loading data (this may take a moment)"))))
   
          (lambda ()    ; try:
            (parameterize ([current-custodian the-cust])
              (define resolved-path (resolve-cache-path cacher full-path subtag))

              (define-values
                (fp local-name enc)
                (cond [(string=? resolved-path full-path)   ; wasn't cached - loading it directly
                       (match-define (list fp nm en) (raw-create-input resolved-path))
                       (define byts (port->bytes fp))
                       (close-input-port fp)
                       (values (open-input-bytes byts) nm en)]
                    
                      [else
                       (values (create-input resolved-path)
                               (lookup-entry-data cacher full-path "real-name")
                               (lookup-entry-data cacher full-path "enc"))]))

              (with-handlers ([exn:fail? (λ (e) (sinbad-error (format "failed to load data: ~a" (exn-message e))))])
                (define zfp (handle-zip fp full-path local-name))
                
                (set! the-data (da-load-data data-factory
                                             (cond [zfp zfp]
                                                   [else resolved-path])
                                             enc))
                (set! sampled? #f)
                (set! loaded? #t)
                (set! random-index #f)  ; so that (fetch-random) actually returns the same position, until (load) is called again
                )))

          (lambda ()     ; finally:
            (when (unbox D) (stop-dot-printer (unbox D)))
            (custodian-shutdown-all the-cust)
            ;; TODO: share  load   usage
            ))
         
         this]))


    (define/public (handle-zip fp full-path local-name)
      (cond
        [(or (smells-like-zip? full-path)
             (and local-name (smells-like-zip? local-name)))
         (define zdir (read-zip-directory fp))
         (define members (map bytes->string/locale (zip-directory-entries zdir)))
         ;(printf "Zip members: ~a~n" members)

         (when (and (not (hash-has-key? option-settings 'file-entry))
                    (= 1 (length members)))
           (set-option! "file-entry" (car members)))

         (define fe-value (hash-ref option-settings 'file-entry #f))
         (define fe-subtag (format "file-entry:~a" fe-value))
         ;(printf "fe-value: ~a~n" fe-value)
         ;(printf "tags: ~a ~a~n" full-path fe-subtag)
         
         (cond
           [(and fe-value (zip-directory-contains? zdir fe-value))
            (define entry-cached-path (resolve-cache-path cacher full-path fe-subtag))
            (cond
              [entry-cached-path   ; the zip entry was previously cached...
               (close-input-port fp)
               (create-input entry-cached-path)]
              [else    ; not previously cached
               (call-with-unzip-entry
                fp fe-value
                (lambda (tmp-entry-path)
                  (call-with-input-file tmp-entry-path	 	 	 	 
                    (lambda (tfp)
                      (cond [(= (cacher-expiration cacher) NEVER-CACHE)
                             ; don't attempt to cache unzipped file if caching disabled
                             (open-input-bytes (port->bytes tfp))]
                            [else  (add-to-cache cacher full-path fe-subtag tfp)
                                   (create-input (resolve-cache-path cacher
                                                                     full-path
                                                                     fe-subtag))])))))])]
            
           [else   ; no fe-value or invalid fe-value
            (sinbad-error (format "Specify a file-entry from the ZIP file: ~a" members))])]
        [else fp]))
    


    (define/public (load-sample! [max-elts 25] [random-seed #f] [force-reload? #f])
#|
        Load and then sample from all available data. See sample_data().
        The sampled data is cached. To reload the entire data and
        resample it, rather than using a previously-cached sample,
        use force_reload=True.
        #
        # look for cache subtag:   "sample:<max-elts>" in the cache
        # if  not there, or if stale, or if force_reload is True:
        #     load()
        #     sample the loaded data
        #     cache the sample (serialized as json)
        #     return the sample
        # otherwise
        #     load the cached sample (unserialize as json)
        #     return it
        #
|#
      (unless connected? (sinbad-error (format "not connected: ~a" path)))
      (unless (ready-to-load?)
        (sinbad-error (format "not ready to load; missing params: ~a"
                              (string-join (map symbol->string (missing-params)) ", "))))

      (define full-path (get-full-path-url))
      (define fe-value (hash-ref option-settings 'file-entry #f))
      (define subtag
        (if fe-value
            (format "sample:~a-~a_~a" fe-value max-elts (or random-seed "x"))
            (format "sample:~a_~a" max-elts (or random-seed "x"))))

      (define D (box #f))
      (define the-cust (make-custodian))

      (dynamic-wind
       (lambda ()
         (set-box! D (dot-printer (format "Sampling data (this may take a moment)"))))
   
       (lambda ()    ; try:
         (parameterize ([current-custodian the-cust])
      
           (define entry-cached-path (resolve-cache-path cacher full-path subtag))

           (cond
             [(and entry-cached-path (not force-reload?))   ; seems to be cached
              (define fp (create-input entry-cached-path))
              (set! the-data (da-load-data (json-access) fp))  ; specifically JSON format

              (set! sampled? #t)
              (set! loaded? #t)   ; duplicate these two lines because (load!) didn't get called on this path of execution
              (set! random-index #f)  ; so that (fetch-random) actually returns the same position, until (load) is called again
              ]

             [else
              (load! force-reload?)
              (when loaded?
                ;(printf "the-data: ~a~n" (substring (format "~a" the-data) 0 (min 1000 (string-length (format "~a" the-data)))))
                (define sampled-data (sample-data the-data max-elts random-seed))
                ;(printf "sampled-data: ~a~n" (substring (format "~a" sampled-data) 0 (min 1000 (string-length (format "~a" sampled-data)))))
                (define fp (open-input-bytes (jsexpr->bytes sampled-data)))
                (add-to-cache cacher full-path subtag fp)
                (set! the-data sampled-data)
                (set! sampled? #t)
                
                ;; TODO: share  sample   usage
                )
              ])
      
           ))

         (lambda ()     ; finally:
           (when (unbox D) (stop-dot-printer (unbox D)))
           (custodian-shutdown-all the-cust)
           ))
       
      this)

    

    (define/public (load-fresh-sample! [max-elts 25] [random-seed #f])
      (load-sample! max-elts random-seed #t))


    (define/public (clear-cache)
      (clear-entire-cache cacher))


    ;;   FETCHING ---------------------------------
    
    (define/public (fetch-all)
      (unless (has-data?) (sinbad-error "no data available - make sure you called (load)"))
      the-data)

    

    (define/public (fetch #:base-path [base-path #f] #:select [select #f] #:apply [func 'dict] . field-paths)
      (let ([select (if (and (eq? 'random select) random-index)
                        random-index
                        select)])
       (cond
        [(empty? field-paths)
         (if base-path
             (fetch #:select select #:apply func base-path)
             (apply-select (fetch-all) select))]
         
        [else 
         (define-values (pref field-paths-suf)
           (if (andmap string? field-paths)
               (extract-common-prefix (map trim/ field-paths))
               (values "" (map trim/ field-paths))))
      
         (define pref-split (string-split pref "/"))
      
         (define base-path-fields (append
                                   (if base-path
                                       (string-split base-path "/")
                                       '())
                                   pref-split))

         (define field-paths-split
           (map (λ(fp)
                  (cond
                    [(string? fp)
                     (define splitted (string-split fp "/"))
                     (if (= 1 (length splitted))
                         (first splitted)
                         (cons 'path splitted))]
                    [else fp])) field-paths-suf))

         (define final-sig
           (apply build-sig (not select) func base-path-fields field-paths-split))
         (dprintf "final-sig: ~s~n" final-sig)
         (parameterize ([invalid-fields-reported (map string->symbol ignore-error-fields)])
           (real-unify (fetch-all) final-sig select #f))])))


    
    (define/public (has-fields? #:base-path [base-path #f] . field-paths)
      (unless (has-data?) (sinbad-error "no data available - make sure you called (load)"))
      
      (with-handlers ([exn:fail? (λ (e) #f)])
        (send/apply this fetch #:base-path base-path field-paths)
        #t))


    (define/public (field-list [base-path #f])
      (unless (has-data?) (sinbad-error "no data available - make sure you called (load)"))

      (with-handlers ([exn:fail? (λ (e) '())])
        (define data (if base-path (fetch base-path) (fetch)))

        (define unwrapped-data
          (let LOOP ([d data])
            (if (list? d)
                (LOOP (first d))
                d)))

        (if (dict? unwrapped-data)
            (map symbol->string (dict-keys unwrapped-data))
            '())))


    (define/public (data-length [base-path #f])
      (unless (has-data?) (sinbad-error "no data available - make sure you called (load)"))

      (with-handlers ([exn:fail? (λ (e) '())])
        (define data (if base-path (fetch base-path) (fetch)))
        (if (list? data) (length data) 0)))


    ;;; --- description  -------------------------------------------------------

    (define/public (description)
      (unless (has-data?) (sinbad-error "no data available - make sure you called (load)"))

      (json-describe the-data))

    (define/public (display-description)
      (printf "-----~n")

      (define url-path (if (ready-to-load?) (get-full-path-url) path))

      (if (and name (not (string=? name path)))
          (printf "Data Source: ~a\nURL: ~a~n" name url-path)
          (printf "Data Source: ~a~n" url-path))

      (when format-type
        (printf "Format: ~a~n" format-type))

      (when (or info-url info-text) (printf "~n"))
      (when info-text (printf "~a~n" info-text))
      (when info-url (printf "(See ~a for more information about this data.)~n" info-url))

      (define param-keys (sort (dict-keys params) symbol-name<=?))
      (unless (empty? param-keys)
        (printf "~nThe following (connection) parameters may/must be set on this data source:~n")

        (for ([p-key param-keys])
          (define prm (dict-ref params p-key))
          (define p-val (dict-ref param-values p-key #f))
          (define desc (if (param-description prm)
                           (format "~n     ~a" (param-description prm))
                           ""))
          (define req (if (param-required? prm) " [*required]" ""))
          (define val-str (if p-val
                              (format "currently set to: ~a" p-val)
                              "not set"))
          (printf "   - ~a (~a)~a~a~n" (symbol->string p-key) val-str req desc)))

      (define opt-keys (sort (da-options data-factory) string<=?))
      (unless (empty? opt-keys)
        (printf "~nThe following options are available for this data source format:~n")
        (for ([o-key opt-keys])
          (define o-val (da-get-option data-factory o-key))
          (define val-str (if o-val (format " (currently set to: \"~a\")" o-val) ""))
          (printf "   - ~a~a~n" o-key val-str)))

      (if (has-data?)
          (printf "~nThe following data is available:~n~a~n" (description))
          (printf "~n*** Data not loaded *** ... use (load)~n~n"))

      this)
    

    ;;; --- various options management methods ---------------------------------

    (define/public (set-cache-timeout! value)
      "Set the cache delay to the given value in seconds"

      (set! cacher (update-timeout cacher (if (> value 0) (* 1000 value) value)))
      this)

    
    (define/public (set-cache-directory! path)
      "Set the cache directory for this data-source (only)"
      (set! cacher (update-directory cacher path))
      this)

    (define/public (cache-directory)
      (cacher-directory cacher))

    (define/public (cache-timeout)     ; provides it in seconds
      (let ([e (cacher-expiration cacher)])
        (if (< 0 e)
            (/ e 1000)
            e)))
    
    (define/public (set-option! name value)
      (define (no-slash? s) (not (string-contains? s "/")))
      
      (match (string-downcase name)
        ["file-entry"
         (set! option-settings (hash-set option-settings 'file-entry value))]
        ["ignore-missing"
         (cond
           ; given as a list of strings
           [(and (list? value) (andmap string? value) (andmap no-slash? value))
            (set! ignore-error-fields (append value ignore-error-fields))]
           [(and (string? value) (no-slash? value))
            (set! ignore-error-fields (cons value ignore-error-fields))]
           [else
            (sinbad-error "ignore-missing option value should be a string or list of strings without / in them")])]
        [else
         (da-set-option! data-factory name value)])
      this)

    (define/public (set-param! name value)
      (set! param-values (hash-set param-values (if (string? name) (string->symbol name) name) value))
      this)

    (define/public (add-param! prm)
      (set! params (hash-set params (string->symbol (param-key prm)) prm))
      this)


    ;; -------------- spec export -----------------------------------------

    (define/public (export [port/path #f])
      "Exports a jsexpr describing this data-source (in the format
        used for specification files).
        
        If a port or file path is provided, the specification
        is saved to the file."

      (define spec (make-hasheq))

      (when path (hash-set! spec 'path path))
      (when name (hash-set! spec 'name name))
      (when format-type (hash-set! spec 'format format-type))
      (when info-url (hash-set! spec 'infourl info-url))
      (when info-text (hash-set! spec 'description info-text))

      (define cache-spec (make-hasheq))
      (hash-set! cache-spec 'timeout
                 (let ([e (cacher-expiration cacher)])
                   (cond [(<= e 0) e]
                         [(integer? (/ e 1000)) (/ e 1000)]
                         [else (exact->inexact (/ e 1000))])))
      (when (not (equal? (cacher-directory cacher) (cacher-directory default-cacher)))
        (hash-set! cache-spec 'directory (cacher-directory cacher)))
      (hash-set! spec 'cache cache-spec)

      (define opt-list '())
      (for ([(k v) (in-dict option-settings)])
        (set! opt-list (cons (hasheq 'name k 'value v) opt-list)))
      (for ([k (da-options data-factory)])
        (define v (da-get-option data-factory k))
        (when v
          (set! opt-list (cons (hasheq 'name k 'value v) opt-list))))
      (hash-set! spec 'options opt-list)

      (define param-list '())
      (for ([(k prm) (in-dict params)])
        (set! param-list (cons (param->jsexpr prm (dict-ref param-values k #f)) param-list)))
      (for ([(k v) (in-dict param-values)])
        (when (not (dict-has-key? params k))
          (define prm (make-param (symbol->string k) 'query))
          (set! param-list (cons (param->jsexpr prm (dict-ref param-values k #f)) param-list))))
      (hash-set! spec 'params param-list)

      (cond
        [(path-string? port/path)
         (with-output-to-file (expand-user-path port/path)
           (lambda () (display (jsexpr->string spec))))]
        [(output-port? port/path)
         (display (jsexpr->string spec) port/path)])
      
      spec)


    

    ;; -------------- data unification -----------------------------------------

    (define (apply-select data select)
      (if (or (not select) (not (list? data)))
          data
          (match select
            [(? nonnegative-integer? i)  (list-ref data i)]
            [(? negative-integer? i)     (list-ref data (+ (length data) i))]
            ['random
             (define selected-index (random-ref (length data)))
             (set! random-index (append (or random-index '()) (list selected-index)))
             (list-ref data selected-index)]
            [(list (? nonnegative-integer? i) is ...) (list-ref data i)]
            [(list (? negative-integer? i) is ...)    (list-ref data (+ (length data) i))]
            [#f                          (list-ref data 0)])))

#|
sig :=    (list <sig>)
     |    (list <function> <sig> ...)
     |    (list 'dict (<name> <sig>) ...)
     |    (list 'path <sig>)
     |    <path-string>
|#

    ;; real-unify : jsexpr sig [integer? or #f or 'random or (list integer? ...)] boolean -> (list any)

    (define (real-unify data sig select as-list?)
      (define upd-select   ; "use-up" the select index
        (match select
          [(list x) #f]
          [(list x xs ...) xs]
          ['random select]      ; let 'random flow through
          [_ #f]))
  
      (match sig
        [(list (? procedure? f) ss ...)
         (dprintf "apply ~a to:\n~a\n" (object-name f) ss)
         (match data
           [(list ds ...)
            (if as-list?
                (flatten (map (λ(d) (real-unify d sig select as-list?)) ds))
                (real-unify (apply-select ds select) sig upd-select as-list?))]
           [(? dict? _)
            (define p-unif (map (λ (s) (real-unify data s select as-list?)) ss))
            (apply f p-unif) ])]

    
        [(list 'dict ss ...)
         (dprintf "dict of ~a~n" ss)
         (match data
           [(list ds ...)
            (if as-list?
                (flatten (map (λ(d) (real-unify d sig select as-list?)) ds))
                (real-unify (apply-select ds select) sig upd-select as-list?))]
           [(? dict? _)
            (define assocs (map (λ(sub)
                                  (define-values (n s)
                                    (match sub
                                      [(list n s) (values n s)]
                                      [(? string? s) (values s s)]))
                                  (cons (if (symbol? n) n (string->symbol n))
                                        (real-unify data s select as-list?)))
                                ss))
            (make-hasheq assocs)])]

    
        [(list 'path p ps ... s)
         (dprintf "traverse ~a ~a~n" p ps)

         (define (traverse data path)
           (match data
             [(? dict? _) (dict-ref/check data p)]
             [(? list? _) (flatten (map (λ(d) (traverse d path)) data))]
             [else (sinbad-error (format "no path to ~a ~a" p ps))]))

         (define p-data (traverse data p))
       
         (if (cons? ps)
             (real-unify p-data (append (cons 'path ps) (list s)) select as-list?)
             (real-unify p-data s select as-list?))]


        [(list 'value v) v]

    
        [(list s)
         (define result
           (match data
             [(list ds ...)
              (dprintf "collecting list~n")
              (map (λ (d) (real-unify d s select #t)) ds)]
             [_
              (dprintf "wrapping list~n")
              (real-unify data s select #t)]))
         (if (and (cons? s) (eq? (car s) list))   ; don't flatten explicit (list ...) constructor requests
             result
             (flatten result))]

    
        [(? string? p)
         ;(printf "contents of ~a at ~a~n" p (list->string (take (string->list (format "~a" data)) 1000)))
         (define result
           (match data
             [(or (? string? _) (? boolean? _) (? number? _)) data]
             [(? dict? _) (let ([r (dict-ref/check data p)])
                            (if select (apply-select r select) r))]
             [(? list? _) (real-unify (apply-select data select) sig upd-select as-list?)]
             [else (sinbad-error "not primitive data")]))
         result]))


    ;; dict  string  ->  any
    ;; this does a dict-ref, but if the key is not in the data,
    ;; then it prints out a warning and produces #f, unless the
    ;; key is already in the (invalid-fields-reported) parameter
    ;; in which case no warning message is printed, and just #f
    ;; is returned
    (define (dict-ref/check data s-key)
      (define key (if (symbol? s-key) s-key (string->symbol s-key)))
      (cond
        [(dict-has-key? data key) (dict-ref data key)]
        [else
         (unless (member key (invalid-fields-reported))
           (fprintf (current-error-port) "warning: missing data for ~a~n" s-key)
           (invalid-fields-reported (cons key (invalid-fields-reported))))
         #f]))

    
    ))











(define (trim/ s)
  (cond [(string? s) (string-trim s "/")]
        [else s]))



;; (listof string) -> values string (listof string)
(define (extract-common-prefix paths)
  ; idea from Python source code for commonprefix()
  (cond
    [(empty? paths) (values "" paths)]
    [else
     (define sorted-p (sort paths string<=?))
     (define min-p (string->list (first sorted-p)))
     (define max-p (string->list (last sorted-p)))
     (define pref (take-common-prefix min-p max-p char=?))

     (define slash-position 
       (for/first ([c (reverse pref)]
                   [i (in-naturals)]
                   #:when (char=? c #\/))
         i))

     (if (not slash-position)
         (values "" paths)
         (let* ([s (list->string pref)]
                [pos (- (string-length s) slash-position)]
                [prefix (substring s 0 pos)])
           (values prefix
                   (map (λ(p) (substring p pos)) paths))))]))

(module+ test
  (check-equal? (let-values ([(a b) (extract-common-prefix (list "a" "b" "c"))]) (list a b))
                (list "" (list "a" "b" "c")))
  (check-equal? (let-values ([(a b) (extract-common-prefix (list "a/a" "b/a" "c/a"))]) (list a b))
                (list "" (list "a/a" "b/a" "c/a")))
  (check-equal? (let-values ([(a b) (extract-common-prefix (list "z/a/a" "z/b/a" "z/c"))]) (list a b))
                (list "z/" (list "a/a" "b/a" "c")))
  (check-equal? (let-values ([(a b) (extract-common-prefix (list "zx/qyc" "zx/qya/a" "zx/qyai/gh" "zx/qyb/a" ))]) (list a b))
                (list "zx/" (list "qyc" "qya/a" "qyai/gh" "qyb/a" )))
  (check-equal? (let-values ([(a b) (extract-common-prefix (list "zx/q/yc" "zx/q/ya/a" "zx/q/yai/gh" "zx/q/yb/a" ))]) (list a b))
                (list "zx/q/" (list "yc" "ya/a" "yai/gh" "yb/a" )))
  )








;; build-sig : boolean proc? (listof <path-string>) [(listof <sig>)] -> <sig>
(define (build-sig as-list? func base-path . field-paths)
  (define func-default? (and (symbol? func) (eq? func 'dict)))
  
  (define sig
    (cond
      [(and func-default? (= 1 (length field-paths)))
       (first field-paths)]
      [func-default?
       (define dict-keys (map (λ (fp) (match fp
                                        [(list 'path fps ...) (string-join fps "-")]
                                        [_ fp]))
                              field-paths))
       (cons func (map list dict-keys field-paths))] ; pairs up for 'dict: (list "path" "path" )
      [else
       (cons func field-paths)]))

  (define (wrap-if-list s)
    (if as-list? (list s) s))
  
  (define final-sig
    (if (empty? base-path)
        (wrap-if-list sig)
        `(path ,@base-path ,(wrap-if-list sig))))

  (dprintf "build-sig (~a): ~a~n" base-path final-sig)
  final-sig)

(module+ test
  (struct quake (title time mag) #:transparent)
  
  (check-equal? (build-sig #t 'dict (list "features" "properties") "title")
                `(path "features" "properties" ("title")))  ; ((dict ("title" "title"))))
  (check-equal? (build-sig #f 'dict (list "features" "properties") "title")
                `(path "features" "properties" "title"))
  (check-equal? (build-sig #t 'dict (list "features" "properties") "title" "time")
                `(path "features" "properties" ((dict ("title" "title") ("time" "time")))))
  (check-equal? (build-sig #t quake (list "features" "properties")
                           "title"
                           (list seconds->date "time")
                           "mag")
                `(path "features" "properties" ((,quake "title" (,seconds->date "time") "mag"))))
  #; ; TODO ....
  (check-equal? (build-sig #t 'dict '() (list (list string->number "features/properties/code")))
                `((,string->number (path "features" "properties" "title"))))
  
  (check-equal? 2 (+ 1 1)))







(define (json-describe thing [indent 0])
  (define indent-amount 2)
  (define (incr s) (+ s indent-amount))
  
  (define spaces (make-string indent #\space))

  (cond
    [(or (boolean? thing) (number? thing) (string? thing))
     "*"]

    [(list? thing)
     (if (empty? thing)
         "empty list"
         (let ([elts (json-describe (first thing) (incr indent))])
           (if (string-contains? elts "\n")
               (string-append spaces "list of:\n" elts)
               (string-append spaces "list of " (string-trim elts)))))]

    [(dict? thing)
     (define keys (sort (dict-keys thing) symbol-name<=?))
     (define key-spaces (make-string (incr indent) #\space))

     (define lines (for/list ([k keys])
                     (define v (dict-ref thing k))
                     (define leader (string-append key-spaces (symbol->string k) " : "))
                     (define v-str (string-trim (json-describe v (string-length leader))))
                     (string-append leader v-str "\n")))

     (string-append spaces "structure with {\n"
                    (string-join lines "")
                    spaces "}")]

    [else
     "?"])

  )

(define symbol-name<=?
  (lambda (s1 s2)
    (string<=? (string-downcase (symbol->string s1))
               (string-downcase (symbol->string s2)))))


;### ----------------------------------------------------------------------

(define (load-spec-file fp)
  "Loads a specification file from the given input-port and
   instantiates and prepares a data-source object based on that."

  (with-handlers ([exn:fail? (λ (e) (sinbad-error (format "failed to load specification file: ~a" (exn-message e))))])
    (define spec (read-json fp))
    (unless (dict-has-key? spec 'path) (error "invalid specification file"))

    (define path (dict-ref spec 'path))
    (define ds (connect path #:format (dict-ref spec 'format #f)))

    (set-field! name ds (dict-ref spec 'name path))
    (set-field! info-url ds (dict-ref spec 'infourl #f))
    (set-field! info-text ds (dict-ref spec 'description #f))

    (when (dict-has-key? spec 'cache)
      (define c (dict-ref spec 'cache))
      (define timeout (dict-ref c 'timeout #f))
      (define directory (dict-ref c 'directory #f))
      (when timeout (send ds set-cache-timeout! timeout))
      (when directory (send ds set-cache-directory! directory)))

    (for ([d (dict-ref spec 'params '())])   ; handle param list
      (define prm (make-param (dict-ref d 'key)
                              (string->symbol (dict-ref d 'type))
                              (dict-ref d 'description #f)
                              (string->boolean/try (dict-ref d 'required #f))))
      (send ds add-param! prm)
      (when (dict-has-key? d 'value)
        (send ds set-param! (dict-ref d 'key) (dict-ref d 'value))))

    (for ([opt (dict-ref spec 'options '())])   ; handle option list
      (send ds set-option! (dict-ref opt 'name) (dict-ref opt 'value)))

    ;(display spec)(newline)
    
    ds))


;### ----------------------------------------------------------------------

#|


(define A
  (send* (connect #:format "json" "http://services.faa.gov/airport/status/ATL")
    (set-param! "format" "application/json")
    ;(get-full-path-url)))
    (load!)
    (fetch-all)))


(define Asub
  (send* (connect #:format "json" "http://services.faa.gov/airport/status/@{airport-code}")
    (add-param! (make-param "airport-code" 'path "3 letter airport code" #t))
    (set-param! "format" "application/json")
    (set-param! "airport-code" "JFK")
    ;(get-full-path-url)))
    (load!)
    (fetch-all)))



(define B
  (send* (connect "https://github.com/tamingtext/book/raw/master/apache-solr/example/exampledocs/books.json")
    (load!)
    (fetch-all)))

(define E
  (send* (connect "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
    (load!)
    (fetch-all)))

(require racket/date)
(struct quake (title time mag) #:transparent)



#;(unify E `(path "features" "properties" (dict ("description" "title") "time" (magnitude "mag"))))
#;(unify E `(path "features" "properties" (,quake "title" (,seconds->date "time") "mag")))
#;(unify E `(path "features" "properties" (,quake "title" (,date->string (,seconds->date "time")) "mag")))
#;(unify E `(path "features" "properties"
                  (,quake "title"
                          (,date->string
                           (,seconds->date (,/ "time" (value 1000)))
                           (value #t))
                          "mag")))




(struct book (title author info) #:transparent)
(struct info (genre categories) #:transparent)

(define B2
  (send* (connect "https://github.com/tamingtext/book/raw/master/apache-solr/example/exampledocs/books.json")
    (load!)
    (fetch #:select 0 #:apply book "name" "author" (list info "genre_s" "cat"))))

(define E2
  (send* (connect "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
    (load!)
    (fetch "properties/title" "geometry/coordinates" "properties/mag"
           #:apply quake #:base-path "features" #:select 0)))

(define E3
  (send* (connect "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
    (load!)
    (fetch "properties/title" "geometry/coordinates" "properties/mag" #:apply quake #:base-path "features" #:select '(1 -1))))

#;
(define E4
  (send* (connect "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
    (load!)
    (fetch (list string->number "features/properties/code"))))


(send* (connect "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
  (load!)
  (display-description))

(send* (connect #:format "json" "http://services.faa.gov/airport/status/ATL")
    (set-param! "format" "application/json")
    (load!)
    (display-description))
|#