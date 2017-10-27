#lang racket

;; THIS CODE IS PROOF-OF-CONCEPT AND NEEDS TO BE RE-ORGANIZED AND DOCUMENTED PROPERLY


(provide (all-defined-out))

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
(1) da-options : data-access -> list[string]
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

(require sinbad/dot-printer)

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
     (parameterize ([current-locale (if enc enc (current-locale))])
       (read-json fp)))])






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
      (list (string->symbol (replace-slash+trim k)) (read-json)))     ; .nah. fix key names to not have /
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
    (random-seed seed))   ; only seed the at the top-level recursive call

  (cond
    [(dict? obj)
     (for/hasheq ([(k v) (in-dict obj)])
       (values k (sample-data v max-elts)))]
     ;(dict-map obj (lambda (k v) (sample-data v max-elts)))]
    [(list? obj)
     (define top-sample (if (<= (length obj) max-elts)
                            obj
                            (select-positions
                             obj
                             (sort (random-sample (length obj) max-elts #:replacement? #f) <))))
     (map (lambda (v) (sample-data v max-elts)) top-sample)]
    [(and (real? obj) (exact? obj)) (exact->inexact obj)]
    [else obj]))





(require  sxml/ssax/SSAX-code)
(require (only-in srfi/13/string string-null?)
         (only-in srfi/1 cons*))

(define res-name->sxml
  (lambda (res-name)
    (string->symbol
     (string-append
      (symbol->string (car res-name))
      ":"
      (symbol->string (cdr res-name))))))


(define (ssax:xml->jsexpr port)
  (let ([result
          ((ssax:make-parser
            
            NEW-LEVEL-SEED 
            (lambda (elem-gi attributes namespaces expected-content seed)
              ;(printf "new-level ~a ~a ~a ~a ~a~n" elem-gi attributes namespaces expected-content seed)
              #f)
            
   
            FINISH-ELEMENT
            (lambda (elem-gi attributes namespaces prior-seed content-seed)
              (define tag (if (symbol? elem-gi) elem-gi (res-name->sxml elem-gi)))
              ;(printf "finish: ~a ~a ~a ~a ~a~n" tag attributes namespaces prior-seed content-seed)

              (let ([content-seed
                     (for/fold ([result content-seed])
                               ([(k v) (in-dict attributes)])
                       (dict-extend/listify
                        result
                        (string->symbol (string-append "@" (symbol->string k)))
                        v))])
                (cond
                  [(empty? prior-seed)   ; root element
                   (string->num/bool/try content-seed)]

                  [(void? prior-seed)    ; empty element - content should be #f
                   (make-hash `((,tag . "")))]
                
                  [(false? prior-seed)   ; element with no significant content, or what? ... not sure
                   (make-hash `((,tag . ,(string->num/bool/try content-seed))))]
                
                  [(dict? prior-seed)
                   (dict-extend/listify prior-seed tag (string->num/bool/try content-seed))]
                
                  [(string? prior-seed)
                   (make-hash `((*content* . ,(string->num/bool/try prior-seed)) (,tag . ,content-seed)))]
                
                  [else (error "bad")]))

              
              )
              

            CHAR-DATA-HANDLER
            (lambda (string1 string2 seed)
              ;(printf "char handler: ~s ~s [~s]~n" string1 string2 seed)
              
              (define str (string-append string1 string2))

              (cond
                [(and (string=? (string-trim str) "")
                      (or (false? seed) (dict? seed))) seed]
                [(false? seed) str]
                [(string? seed) (string-append seed str)]
                [(dict? seed)  (dict-extend/listify seed '*content* (string-trim str))]))

            ;; not really used...
            DOCTYPE
	     (lambda (port docname systemid internal-subset? seed)
	       (values #f '() '() seed))

	     UNDECL-ROOT
	     (lambda (elem-gi seed)
	       (values #f '() '() seed))

             PI
             ((*DEFAULT* .
                         (lambda (port pi-tag seed)
                           (define body (ssax:read-pi-body-as-string port))
                           ; (printf "PI: ~a ~a ~a~n" pi-tag body seed)
                           seed)))
             )
           port '())])
    result))


(define (dict-extend/listify dict key value)
  (cond
    [(string? dict) (dict-extend/listify (make-hash `((*content* . ,dict))) key value)]
    [(not (dict-has-key? dict key))
     (hash-set! dict key value)]
    [(list? (dict-ref dict key))
     (define val-list (dict-ref dict key))
     (hash-set! dict key (append val-list (list value)))]
    [else
     (define existing-val (dict-ref dict key))
     (hash-set! dict key (list existing-val value))])
  dict)

(define (h->l x)
    (if (hash? x)
        (for/list ([(k v) (in-dict x)])
          (if (list? v)
              (cons k (map h->l v))
              (cons k (h->l v))))
        x))

(define (xtest str)
  ;(printf "------- Testing: ~a~n" str)
  
  (let ([result (ssax:xml->jsexpr (open-input-string str))])
    (h->l result)))

;(xtest "<zippy><pippy pigtails=\"2\">ab duh</pippy>cd hi <no /></zippy>")


(module+ test
  (require rackunit)

  )

(module+ test
  
  (check-equal? (xtest "<root>hello</root>") "hello")
  (check-equal? (xtest "<root>hello &amp; &quot; char ents</root>") "hello & \" char ents")
  (check-equal? (xtest "<root>hello<car>Ford</car></root>") `((*content* . "hello") (car . "Ford") ))
  (check-equal? (xtest "<root><car>Ford</car>hello</root>") `((*content* . "hello") (car . "Ford") ))
  (check-equal? (xtest "<root><car><make>Ford</make><model>Taurus</model></car>
                         <car><make>Honda</make><model>Odyssey</model></car></root>")
                '((car . (((model . "Taurus") (make . "Ford"))
                          ((model . "Odyssey") (make . "Honda"))))))
  (check-equal? (xtest "<root><car year=\"1998\"><make>Ford</make><model>Taurus</model></car>
                         <car year=\"2005\"><make>Honda</make><model>Odyssey</model></car></root>")
                '((car . (( (model . "Taurus") (make . "Ford") (@year . "1998"))
                          ( (model . "Odyssey") (make . "Honda") (@year . "2005"))))))
  
  (check-equal? (h->l (dict-extend/listify (make-hash) 'hi "new")) '((hi . "new")))
  (check-equal? (h->l (dict-extend/listify (make-hash '((bye . "old"))) 'hi "new"))
                '((hi . "new") (bye . "old")))
  (check-equal? (h->l (dict-extend/listify (make-hash '((hi . "new"))) 'hi "old"))
                '((hi . ("new" "old"))))
  (check-equal? (h->l (dict-extend/listify (make-hash '((bye . "old") (hi . ("new" "old")))) 'hi "good"))
                '((hi . ("new" "old" "good")) (bye . "old"))))


(define-struct xml-infer ()
  #:methods gen:data-infer
  [(define (matched-by? inf path)
     (let ([path (string-downcase path)])
       (or (string-suffix? path "xml")
           (for/or ([ptrn `(".xml" "=xml" "/xml")])
             (string-contains? path ptrn)))))
   (define (infer-options inf)
     '())])

(define-struct xml-access ()
  #:methods gen:data-access
  [(define (da-options da) '())
   (define (da-get-option da k) #f)
   (define (da-set-option! da k v) (void))
   (define (da-load-data da fp [enc #f])
     (parameterize ([current-locale (if enc enc (current-locale))])
       (ssax:xml->jsexpr fp)))])


;; ------  CSV ------------------------------------------
;; ------  CSV ------------------------------------------
;; ------  CSV ------------------------------------------
;; ------  CSV ------------------------------------------



(require csv-reading)

(define HEADER-OPT     "header"    )
(define SKIP-ROWS-OPT  "skip-rows" )
(define DELIMITER-OPT  "delimiter" )

(define (make-csv-infer [delim #f])
  (csv-infer delim))

(struct csv-infer (delim)
  #:transparent #:mutable
  #:methods gen:data-infer
  [
   (define (matched-by? inf path)
     (let ([path (string-downcase path)])
       (define matches-csv (or (string-suffix? path "csv")
                               (for/or ([ptrn `(".csv" "=csv" "/csv")])
                                 (string-contains? path ptrn))))
       (define matches-tsv (or (string-suffix? path "tsv")
                               (for/or ([ptrn `(".tsv" "=tsv" "/tsv")])
                                 (string-contains? path ptrn))))

       (when matches-tsv
         (set-csv-infer-delim! inf #\tab))

       (or matches-csv matches-tsv)))
   
   (define (infer-options inf)
     (define delim (csv-infer-delim inf))
     (if delim
         (hasheq DELIMITER-OPT delim)
         '()))])

(struct csv-access ([field-names #:auto] [delimiter #:auto] [skip-rows #:auto])
  #:transparent #:mutable
  #:methods gen:data-access
  [(define (da-options da) (list HEADER-OPT SKIP-ROWS-OPT DELIMITER-OPT))
   
   (define (da-get-option da k)
     (cond
       [(string=? k HEADER-OPT)
        (if (csv-access-field-names da)
            (string-join (csv-access-field-names da) ",")
            #f)]
       [(string=? k DELIMITER-OPT)
        (and (csv-access-delimiter da)
             (format "~a" (csv-access-delimiter da)))]
       [(string=? k SKIP-ROWS-OPT)
        (or (csv-access-skip-rows da) 0)]       
       [else #f]))
   
   (define (da-set-option! da k v)
     (cond
       [(string=? k HEADER-OPT)
        (cond [(string? v) (set-csv-access-field-names! da (map string-trim (string-split v ",")))]
              [(and (list? v) (andmap string? v)) (set-csv-access-field-names! da v)]
              [else (raise-arguments-error 'set-option "header value must be provided as a comma-separated string or a list of strings")])]
       [(string=? k DELIMITER-OPT)
        (cond [(char? v) (set-csv-access-delimiter! da v)]
              [(and (string? v) (= 1 (string-length v))) (set-csv-access-delimiter! da (string-ref v 0))]
              [else (raise-arguments-error 'set-option "delimiter value must be a single character")])]
       [(string=? k SKIP-ROWS-OPT)
        (cond [(nonnegative-integer? v) (set-csv-access-skip-rows! da v)]
              [(and (string? v) (string->number v)) (set-csv-access-skip-rows! da (string->number v))]
              [else (raise-arguments-error 'set-option "skip-rows value must be a non-negative integer")])]       
       [else (void)]))
   
   (define (da-load-data da fp [enc #f])
         (parameterize ([current-locale (if enc enc (current-locale))])
           (csv->jsexpr da fp)))])


(define (csv->jsexpr csv-acc fp)
  (define field-syms (and (csv-access-field-names csv-acc)
                          (fix-headers (csv-access-field-names csv-acc))))
  (define skip-rows (or (csv-access-skip-rows csv-acc) 0))

  (define sep-char (if (csv-access-delimiter csv-acc)
                       (list (csv-access-delimiter csv-acc))
                       (list #\,)))
  (define reader (make-csv-reader fp `((separator-chars . ,sep-char))))
  (define raw-rows (drop (csv->list reader) skip-rows))

  (define headers
    (or field-syms (fix-headers (first raw-rows))))
  (define rows (if field-syms raw-rows (rest raw-rows)))

  ;(display headers)(newline)

  ;(hasheq 'data
          (for/list ([row rows])
            (for/hasheq ([cell row]
                         [key  headers])
              (values key (string->num/bool/try cell)))))


(define (replace-slash+trim s)
  (string-replace (string-trim s) "/" "-"))


;; listof-string -> listof-symbol
(define (fix-headers los)
  (for/list ([s los]
             [i (in-naturals)])
    (define s2 (replace-slash+trim s))
    (define s3 (if (string=? "" s2)
                   (format "col-~a" i)
                   s2))
    (string->symbol s3)))


(define (string->num/bool/try s)
  (cond
    [(string? s)
     (define as-num (string->number s))
     (or as-num (string->boolean/try s))]
    [else s]))


;; string -> boolean or string
(define (string->boolean/try s)
  (cond
         [(not (string? s)) s]
         [(string=? (string-downcase s) "true") #t]
         [(string=? (string-downcase s) "false") #f]
         [else s]))



