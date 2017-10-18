#lang racket


(require net/uri-codec
         racket/random)
(require "cacher.rkt"
         "dot-printer.rkt"
         "plugin.rkt"
         "util.rkt")

(module+ test
  (require rackunit))



(provide data-source%
         (struct-out exn:fail:sinbad)
         connect)




(define *predefined-plugins*
  (list (hasheq 'name "JSON (built-in)" 'type-ext "json" 
                'data-infer (json-infer) 'data-factory json-access)))


(define (connect path [data-format #f])
  (define type-ext (and data-format (string-downcase data-format)))

  (or
   
   (for/first ([p *predefined-plugins*]
               #:when (or (and type-ext (string=? type-ext (dict-ref p 'type-ext)))
                          (and (not type-ext) (matched-by? (dict-ref p 'data-infer) path))))
    (new data-source% (name path) (path path) (type-ext type-ext) (plugin p)))
   
   (error
    (if type-ext
       (format "no data source plugin for type ~a" type-ext)
       (format "could not infer data format for ~a" path)))))


(struct exn:fail:sinbad exn:fail ()
  #:extra-constructor-name make-exn:fail:sinbad
  #:transparent)

(define-syntax sinbad-error
  (syntax-rules ()
    [(sinbad-error msg) (raise (make-exn:fail:sinbad msg (current-continuation-marks)))]))


(define *debug* #f)

(define-syntax dprintf
  (syntax-rules ()
    [(dprintf blah ...) (when *debug* (printf blah ...))]))


(define data-source%
  (class object%
    (super-new)
    ;(inspect #f)
    
    (init-field name
                path
                [(format-type type-ext)])
    (init plugin)
    
    (field
     [info-url    #f]
     [info-text   #f])

    ;; private fields ----------------------------------------------------------
    (define cacher       default-cacher)
    (define connected?   (and path #t))
    (define load-ready?  #f)
    (define loaded?      #f)

    (define data-infer (dict-ref plugin 'data-infer))
    (define data-factory ((dict-ref plugin 'data-factory)))

    (define the-data #f)
    (define sampled?     #f)   ; whether the-data is a sample of the actual data

    (define random-index #f)

    ; all the following hash tables assume *symbols* as the key
    
    (define option-settings (hasheq))  ; generic options for the data-source; the data-factory
                                       ; also maintains its own set of options

    ; params are connection-related parameters,
    ; either in a query string or filling in part of the path
    (define params (hasheq))   ; keeps track of *all* parameters information available for this data source
    (define param-values (hasheq)) ; # keeps track of the values of the supplied parameters
    
    

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
        (for/list ([k param-keys])
          (define v (dict-ref param-values k))
          ; TODO -- only use   query-params *********************
          (cons k v)))

      (define full-path-non-subst
        (if (= 0 (dict-count query-params))
            path
            (string-append path "?" (alist->form-urlencoded query-params))))

      ; TODO: substitute path params **********
      (define full-path full-path-non-subst)

      full-path)

    
    (define/private (ready-to-load?)
      (set! load-ready? (or load-ready? (= 0 (length (missing-params)))))
      (and connected? load-ready?))

    
    (define/private (missing-params)
      '())   ; TODO


    (define/public (load [force-reload? #f])
      (when (not connected?) (sinbad-error (format "not connected: ~a" path)))
      (when (not (ready-to-load?)) (sinbad-error (format "not ready to load; missing params: ~a" (missing-params))))

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
         (define-values (fp zfp) (values #f #f))
         
         (dynamic-wind
          (lambda ()
            (set-box! D (dot-printer (format "Loading data (this may take a moment)"))))
   
          (lambda ()    ; try:
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
            
            (set! the-data (da-load-data data-factory
                                         (cond [fp fp]
                                               [else resolved-path])
                                         enc))
            (set! sampled? #f)
            (set! loaded? #t)
            (set! random-index #f)  ; so that (fetch-random) actually returns the same position, until (load) is called again
            )

          (lambda ()     ; finally:
            (when (unbox D) (stop-dot-printer (unbox D)))
            (when fp (close-input-port fp))
            (when zfp (close-input-port zfp))
            ;; TODO: share  load   usage
            ))
         
         this]))


    
    (define/public (fetch-all)
      (unless (has-data?) (sinbad-error "no data available - make sure you called (load)"))
      the-data)

    

    (define/public (fetch #:base-path [base-path #f] #:select [select #f] #:apply [func 'dict] . field-paths)

      (define-values (pref field-paths-suf)
        (if (andmap string? field-paths)
            (extract-common-prefix field-paths)
            (values "" field-paths)))
      
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
        (apply build-sig `(,(not select) ,func ,base-path-fields ,@field-paths-split)))
      (printf "final-sig: ~s~n" final-sig)
      (real-unify (fetch-all) final-sig select #f))
    
    ))



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



;; build-sig : proc? (listof <path-string>) [(listof <sig>)] -> <sig>
(define (build-sig as-list? func base-path . field-paths)
  (define func-default? (and (symbol? func) (eq? func 'dict)))
  
  (define sig
    (cond
      [(and func-default? (= 1 (length field-paths)))
       (first field-paths)]
      [func-default?
       (cons func (map list field-paths field-paths))] ; pairs up for 'dict: (list "path" "path" )
      [else
       (cons func field-paths)]))

  (define (wrap-if-list s)
    (if as-list? (list s) s))
  
  (define final-sig
    (if (empty? base-path)
        (wrap-if-list sig)
        `(path ,@base-path ,(wrap-if-list sig))))

  final-sig)


(module+ test
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
  
  (check-equal? 2 (+ 1 1)))



(define A
  (send* (connect "http://services.faa.gov/airport/status/ATL?format=application/json")
    (load)
    (fetch-all)))

(define B
  (send* (connect "https://github.com/tamingtext/book/raw/master/apache-solr/example/exampledocs/books.json")
    (load)
    (fetch-all)))

(define E
  (send* (connect "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
    (load)
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





#|
sig :=    (list <sig>)
     |    (list <function> <sig> ...)
     |    (list 'dict (<name> <sig>) ...)
     |    (list 'path <sig>)
     |    <path-string>
|#

;; unify : jsexpr sig [integer? or #f or 'random or ...TODO]  -> (list any)
(define (unify data sig [select #f] [as-list #f])
  (real-unify data sig select as-list))

(define (real-unify data sig select as-list)
  
  (define (apply-select ds select)
    (if (not (list? ds))
        ds
        (match select
          [(? nonnegative-integer? i)  (list-ref ds i)]
          [(? negative-integer? i)     (list-ref ds (+ (length ds) i))]
          ['random                     (random-ref ds)]
          [(list (? nonnegative-integer? i) is ...) (list-ref ds i)]
          [(list (? negative-integer? i) is ...)    (list-ref ds (+ (length ds) i))]
          [#f                          (list-ref ds 0)])))

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
        (if as-list
            (flatten (map (λ(d) (real-unify d sig select as-list)) ds))
            (real-unify (apply-select ds select) sig upd-select as-list))]
       [(? dict? _)
        (define p-unif (map (λ (s) (real-unify data s select as-list)) ss))
        (apply f p-unif) ])]

    
    [(list 'dict ss ...)
     (dprintf "dict of ~a~n" ss)
     (match data
       [(list ds ...)
        (if as-list
            (flatten (map (λ(d) (real-unify d sig select as-list)) ds))
            (real-unify (apply-select ds select) sig upd-select as-list))]
       [(? dict? _)
        (define assocs (map (λ(sub)
                              (define-values (n s)
                                (match sub
                                  [(list n s) (values n s)]
                                  [(? string? s) (values s s)]))
                              (cons (if (symbol? n) n (string->symbol n))
                                    (real-unify data s select as-list)))
                            ss))
        (make-hasheq assocs)])]

    
    [(list 'path p ps ... s)
     (dprintf "traverse ~a ~a~n" p ps)

     (define (traverse data path)
       (match data
         [(? dict? _) (dict-ref data (if (symbol? p) p (string->symbol p)))]
         [(? list? _) (map (λ(d) (traverse d path)) data)]
         [else (sinbad-error (format "no path to ~a ~a" p ps))]))

     (define p-data (traverse data p))
       
     (if (cons? ps)
         (real-unify p-data (append (cons 'path ps) (list s)) select as-list)
         (real-unify p-data s select as-list))]


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
     (flatten result)]

    
    [(? string? p)
     (dprintf "contents of ~a at ~a~n" p data)
     (define result
       (match data
         [(or (? string? _) (? boolean? _) (? number? _)) data]
         [(? dict? _) (let ([r (dict-ref data (string->symbol p))])
                        (if select (apply-select r select) r))]
         [(? list? _) (real-unify (apply-select data select) sig upd-select as-list)]
         [else (sinbad-error "not primitive data")]))
     result]))


(struct book (title author info) #:transparent)
(struct info (genre categories) #:transparent)

(define B2
  (send* (connect "https://github.com/tamingtext/book/raw/master/apache-solr/example/exampledocs/books.json")
    (load)
    (fetch #:select 0 #:apply book "name" "author" (list info "genre_s" "cat"))))

(define E2
  (send* (connect "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
    (load)
    (fetch "properties/title" "geometry/coordinates" "properties/mag"
           #:apply quake #:base-path "features" #:select 0)))

(define E3
  (send* (connect "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
    (load)
    (fetch "properties/title" "geometry/coordinates" "properties/mag" #:apply quake #:base-path "features" #:select '(1 -1))))

