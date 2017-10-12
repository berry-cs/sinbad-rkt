#lang racket

(provide all-defined-out)

#|
Generic interface for plug-in modules

Every "plug-in" must provide an <Infer> object and a <DataFactory> object
definition.

A plug-in is represented as a dictionary:
    {  'name : string
       'type-ext : string
       'data-infer : data-infer?
       'data-factory : ( -> data-access?) }

Note: the data-infer object is an instantiated object of the data-infer generic interface;
      the factory is a constructor for a data access object
       
<Infer> objects must provide 
   (1) matched_by : string -> boolean
       which takes the primary path (URL/file name) to the data.
   (2) infer-options : -> dict[string -> string]

The options may be populated in the process of matching and are
then passed on to the data access factory object when it is 
instantiated (for example, the CSV plugin may infer a delimiter
based on whether the path name contains .csv or .tsv).

<DataAccess> objects should support methods:        
(1) da-options : data-access -> list
    produces a list of value option names for the data format
(2) da-get-option : data-access string -> value
    produces the value (or None) of the option with the given name
(3) da-set-option! : data-access string value -> void
(4) da-load_data : data-access input-port [, string] -> jsexpr?
    - the input-port should provide bytes? data (i.e. opened in binary mode)
    - encoding may be provided, e.g. 'utf-8'
    - the returned thing should be a jsexpr?

|#

(require racket/generic)

(require "dot-printer.rkt")

(define-generics data-infer
  (matched-by? data-infer path)
  (infer-options data-infer))

(define-generics data-access
  (da-options data-access)
  (da-get-option data-access key)
  (da-set-option! data-access key val)
  (da-load-data data-access fp [enc]))




(define-struct json-infer ()
  #:methods gen:data-infer
  [(define (matched-by? inf path)
     (let ([path (string-downcase path)])
       (or (string-suffix? path "json")
           (for/or ([ptrn `(".json" "=json" ".geojson" "=geojson" "/json" "/geojson")])
             (string-contains? path ptrn)))))
   (define (infer-options inf)
     '())])

(define-struct json-access ()
  #:methods gen:data-access
  [(define (da-options da) '())
   (define (da-get-option da k) #f)
   (define (da-set-option! da k v) (void))
   (define (da-load-data da fp [enc #f])
     (define D (box #f))
     
     (dynamic-wind
      (lambda ()
        (set-box! D (dot-printer (format "Loading data (this may take a moment)"))))
   
      (lambda ()    ; try:
        (if enc
            (parameterize ([current-locale enc])
              (read-json fp))
            (read-json fp)))

      (lambda ()     ; finally:
        (when (unbox D) (stop-dot-printer (unbox D))))))])






;; MODIFIED FROM:
;; https://github.com/racket/racket/blob/master/racket/collects/json/main.rkt

(require syntax/readerr)

(define (read-json [i (current-input-port)] #:null [jsnull 'null])
  (read-json* 'read-json i jsnull))

(define (read-json* who i jsnull)
  ;; Follows the specification (eg, at json.org) -- no extensions.
  ;;
  (define (err fmt . args)
    (define-values [l c p] (port-next-location i))
    (raise-read-error (format "~a: ~a" who (apply format fmt args))
                      (object-name i) l c p #f))
  (define (skip-whitespace) (regexp-match? #px#"^\\s*" i))
  ;;
  ;; Reading a string *could* have been nearly trivial using the racket
  ;; reader, except that it won't handle a "\/"...
  (define (read-string)
    (define result (open-output-bytes))
    (let loop ()
      (define esc
        (let loop ()
          (define c (read-byte i))
          (cond
            [(eof-object? c) (err "unterminated string")]
            [(= c 34) #f]               ;; 34 = "
            [(= c 92) (read-bytes 1 i)] ;; 92 = \
            [else (write-byte c result) (loop)])))
      (cond
        [(not esc) (bytes->string/utf-8 (get-output-bytes result))]
        [(assoc esc '([#"b" . #"\b"] [#"n" . #"\n"] [#"r" . #"\r"]
                                     [#"f" . #"\f"] [#"t" . #"\t"]
                                     [#"\\" . #"\\"] [#"\"" . #"\""] [#"/" . #"/"]))
         => (λ (m) (write-bytes (cdr m) result) (loop))]
        [(equal? esc #"u")
         (let* ([e (or (regexp-try-match #px#"^[a-fA-F0-9]{4}" i)
                       (err "bad string \\u escape"))]
                [e (string->number (bytes->string/locale (car e)) 16)])
           (define e*
             (if (<= #xD800 e #xDFFF)
                 ;; it's the first part of a UTF-16 surrogate pair
                 (let* ([e2 (or (regexp-try-match #px#"^\\\\u([a-fA-F0-9]{4})" i)
                                (err "bad string \\u escape, ~a"
                                     "missing second half of a UTF16 pair"))]
                        [e2 (string->number (bytes->string/locale (cadr e2)) 16)])
                   (if (<= #xDC00 e2 #xDFFF)
                       (+ (arithmetic-shift (- e #xD800) 10) (- e2 #xDC00) #x10000)
                       (err "bad string \\u escape, ~a"
                            "bad second half of a UTF16 pair")))
                 e)) ; single \u escape
           (write-string (string (integer->char e*)) result)
           (loop))]
        [else (err "bad string escape: \"~a\"" esc)])))
  ;;
  (define (read-list what end-rx read-one)
    (skip-whitespace)
    (if (regexp-try-match end-rx i)
        '()
        (let loop ([l (list (read-one))])
          (skip-whitespace)
          (cond [(regexp-try-match end-rx i) (reverse l)]
                [(regexp-try-match #rx#"^," i) (loop (cons (read-one) l))]
                [else (err "error while parsing a json ~a" what)]))))
  ;;
  (define (read-hash)
    (define (read-pair)
      (define k (read-json))
      (unless (string? k) (err "non-string value used for json object key"))
      (skip-whitespace)
      (unless (regexp-try-match #rx#"^:" i)
        (err "error while parsing a json object pair"))
      (list (string->symbol k) (read-json)))
    (apply hasheq (apply append (read-list 'object #rx#"^}" read-pair))))
  ;;
  (define (read-json [top? #f])
    (skip-whitespace)
    (cond
      [(and top? (eof-object? (peek-char i))) eof]
      [(regexp-try-match #px#"^true\\b"  i) #t]
      [(regexp-try-match #px#"^false\\b" i) #f]
      [(regexp-try-match #px#"^null\\b"  i) jsnull]
      [(regexp-try-match
        #rx#"^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?" i)
       => (λ (bs) (string->number (bytes->string/utf-8 (car bs))))]
      [(regexp-try-match #rx#"^[\"[{]" i)
       => (λ (m)
            (let ([m (car m)])
              (cond [(equal? m #"\"") (read-string)]
                    [(equal? m #"[")  (read-list 'array #rx#"^\\]" read-json)]
                    [(equal? m #"{")  (read-hash)])))]
      [else (err "bad input")]))
  ;;
  ;; if there is more data after the first JSON object,
  ;; see if more objects can be decoded, and collect them in a list
  (define result (let accum ([res '()])
                   (define obj (read-json #t))
                   (if (eof-object? obj)
                       res
                       (accum (cons obj res)))))
  (if (= 1 (length result))
      (first result)
      (reverse result)))



(require racket/random)

(define (select-positions lst pos)
  (reverse
   (let loop ([lst lst]
              [pos pos]
              [cur 0]
              [result '()])
     (cond
       [(or (empty? lst) (empty? pos)) result]
       [(> cur (first pos)) result]
       [(= cur (first pos)) (loop (rest lst) (rest pos) (add1 cur) (cons (first lst) result))]
       [else (loop (rest lst) pos (add1 cur) result)]))))
  

(define (sample-data obj max-elts [seed #f])
  (when seed
    (random-seed seed))

  (cond
    [(dict? obj)
     (dict-map obj (lambda (k v) (sample-data v max-elts)))]
    [(list? obj)
     (define top-sample (if (<= (length obj) max-elts)
                            obj
                            (select-positions
                             obj
                             (sort (random-sample (length obj) max-elts #:replacement? #f) <))))
     (map (lambda (v) (sample-data v max-elts)) top-sample)]
    [else obj]))
