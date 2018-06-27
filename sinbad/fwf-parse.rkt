#lang racket

(module+ test
  (require test-engine/racket-tests))

(provide run-on-file
         (struct-out fwf))

(require 2htdp/batch-io)
(require (only-in 2htdp/image
                  beside above square save-image))


(module+ test
  (check-expect (filter-blank (list "  " "   fa   " "afeji" "\t   \t" "gg"))
                (list "   fa   " "afeji" "gg")))

(define (filter-blank a-los)
  (filter (λ(s) (not (regexp-match-exact? #px"\\s*" s))) a-los))


(define (extend-to str desired-length)
  (define diff (- desired-length (string-length str)))
  (if (<= diff 0)
      str
      (string-append str (make-string diff #\space))))


(define (line->e-vector line)
  (for/vector ([ch line])
      (if (char-whitespace? ch) 0 1)))

(define (e-vector->line vec)
  (list->string (for/list ([v vec]) (if (zero? v) #\space #\x))))

(define (lines->e-matrix lines)
  (define max-line-length (add1 (apply max (map string-length lines))))
  (for/vector ([line (append (list "") lines (list ""))])
    (line->e-vector (string-append " " (extend-to line max-line-length)))))

(define (fluff-e-matrix mtx)
  (for ([row-index (vector-length mtx)])
    (define row (vector-ref mtx row-index))
    (for ([col-index (vector-length row)])
      (define cell (vector-ref row col-index))
      (when (and (zero? cell)
                 (< 0 row-index (sub1 (vector-length mtx)))
                 (< 0 col-index (sub1 (vector-length row))))
        (when (<= 3 (count-one-nbrs mtx row-index col-index))
          (vector-set! row col-index 2))

        )))
  mtx)

        

(define (count-one-nbrs mtx r c)
  (define (get i j)
    (vector-ref (vector-ref mtx i) j))
  (apply +
         (map (λ(cell) (if (= 1 cell) 1 0))
              (list (get (sub1 r) c)
                    (get (add1 r) c)
                    (get r (sub1 c))
                    (get r (add1 c))))))



(define (matrix->bitmap mtx [dot-size 5])
    (for/fold ([img (square 0 "solid" "white")])
              ([row mtx])
      (above
       img
       (for/fold ([row-img (square 0 "solid" "white")])
                 ([cell row])
         (beside row-img
                 (square dot-size "solid" (match cell
                                     [0 "white"]
                                     [1 "black"]
                                     [2 "darkslategray"]
                                     [_ "darkgray"])))))))


;; [vector [vector number]] number number -> [vector number]
(define (matrix-flatten mtx [start-row 0] [num-rows -1])
  (define mtx-select (if (< num-rows 0)
                         (vector-drop mtx start-row)
                         (vector-take (vector-drop mtx start-row) num-rows)))
                          
  (or
   (for/fold
    ([result #f])
    ([row mtx-select])
     (if (not result)
         row
         (vector-map + result row)))
   '()))



(define (extract-group-positions line)
  (regexp-match-positions* #px"\\S+" line))



(module+ test
  (check-expect (overlapping-group? '(1 . 2) '(1 . 3)) #t)
  (check-expect (overlapping-group? '(10 . 20) '(5 . 15)) #t)
  (check-expect (overlapping-group? '(10 . 20) '(15 . 25)) #t)
  (check-expect (overlapping-group? '(15 . 25) '(10 . 20)) #t)
  (check-expect (overlapping-group? '(10 . 20) '(15 . 19)) #t)
  (check-expect (overlapping-group? '(15 . 19) '(10 . 20)) #t)
  (check-expect (overlapping-group? '(1 . 2) '(2 . 3)) #f)
  (check-expect (overlapping-group? '(1 . 2) '(-4 . 1)) #f))

(define (overlapping-group? g1 g2)
  (or (and (<= (car g1) (car g2)) (< (car g2) (cdr g1)))
      (and (<= (cdr g2) (cdr g1)) (< (car g1) (cdr g2)))
      (and (<= (car g1) (car g2)) (<= (cdr g2) (cdr g1)))
      (and (<= (car g2) (car g1)) (<= (cdr g1) (cdr g2)))))

(define (group-union g1 g2)
  (cons (min (car g1) (car g2)) (max (cdr g1) (cdr g2))))

(define (union-groups groups-list)
  (sort
   (for/fold ([union '()])
             ([group (apply append groups-list)])
     ; see if group can be unioned into any existing group in the union
     (define non-overlapping (filter (λ(g) (not (overlapping-group? g group))) union))
     (define overlapping (filter (λ(g) (overlapping-group? g group)) union))
     (define all-unioned (for/fold ([o-union group])
                                   ([g overlapping])
                           (group-union o-union g)))
     (cons all-unioned non-overlapping))
   (λ(g1 g2)
     (<= (car g1) (car g2)))))


(define (stretch-group-positions groups-list)
  (for/fold ([result #f])
            ([groups groups-list])
    (if (not result)
        groups
        (for/list ([acc-group result]
                   [cur-group groups])
          (cons (min (car cur-group) (car acc-group))
                (max (cdr cur-group) (cdr acc-group)))))))


(define (apply-group group line)
  (substring line
             (min (car group) (string-length line))
             (min (cdr group) (string-length line))) )

(define (apply-groups groups-list line)
  (for/list ([group groups-list]) (apply-group group line)))




(define (check&merge-groups groups-list lines [threshold-pct .2])
  (define (non-empty-count group)
      (for/sum ([line lines])
        (define str (apply-group group line))
        (if (regexp-match-exact? #px"\\s*" str) 0 1)))

  (define to-remove
    (for/list ([group groups-list]
               #:when (< (/ (non-empty-count group) (length lines)) threshold-pct))
      (displayln `(remove ,group))
      group))

  (define (closest-other-to group)
    (argmin (λ(g) (cond [(equal? g group) +inf.0]
                        [(overlapping-group? g group) 0]
                        [else (min (abs (- (car g) (cdr group)))
                                   (abs (- (cdr g) (car group)))) ]))
            groups-list))

  (define removed
    (for/fold ([result groups-list])
              ([group to-remove])
      (filter (λ(g) (not (equal? g group))) result)))

  (define merged
    (for/fold ([result removed])
              ([group to-remove])
      (define closest-group (closest-other-to group))
      (map (λ(g) (if (equal? g closest-group)
                     (group-union g group)
                     g))
           result)))
  
  merged)

; Pair (listof Pair) -> Number

(module+ test
  (check-expect (group-overlap-count '(5 . 10) '((1 . 4) (7 . 12) (15 . 20)))
                1))

(define (group-overlap-count group groups)
  (length (filter (λ(g) (overlapping-group? group g)) groups)))


; (listof Pair) (listof Pair) -> Number
; the number of minimum/maximum overlaps between any pair in g1 and g2
(define (overlap-min/max-counts g1 g2)
  (define all-counts (append
                      (for/list ([g g1]) (group-overlap-count g g2))
                      (for/list ([g g2]) (group-overlap-count g g1))))
  (cons (apply min all-counts) (apply max all-counts)))
  


; (listof Pair)  (listof (listof Pair)) -> Boolean
(define (exactly-single-overlaps? groups groups-list)
  (define-values (the-min the-max)
    (for/fold ([the-min +inf.0]
               [the-max -inf.0])
              ([ref-groups groups-list])
      (define min.max (overlap-min/max-counts groups ref-groups))
      (values (min (car min.max) the-min)
              (max (cdr min.max) the-max))))
  (= 1 the-min the-max))
    
  
  ;(for/or ([group groups]) (let ([c (group-overlap-count group groups-list)])
  ;                           (or (= c 0) (> c 1)))))

(module+ test
  (check-expect (remove-spurious '(((1 . 10) (14 . 20) (25 . 32) (33 . 40))
                                   ((1 . 11) (15 . 20) (26 . 30) (32 . 39))
                                   ((2 . 9) (16 . 18) (26 . 33) (35 . 39))
                                   ((5 . 16) (17 . 20) (26 . 30) (31 . 40))
                                   ((1 . 9) (15 . 18) (26 . 30) (32 . 39))
                                   ))
                '(((1 . 9) (15 . 18) (26 . 30) (32 . 39))
                  ((1 . 11) (15 . 20) (26 . 30) (32 . 39))
                  ((1 . 10) (14 . 20) (25 . 32) (33 . 40))
                  ; ((2 . 9) (16 . 18) (26 . 33) (35 . 39)) ; gone
                  ; ((5 . 16) (17 . 20) (26 . 30) (31 . 40)) ; gone
                  )))

;  (list (list 2 2) (list 5 5) (list 7))  5  => (list (list 2 2) (list 5 5 5) (list 7))
;  (list (list 2 2) (list 5 5) (list 7))  8  => (list (list 2 2) (list 5 5) (list 7) (list 8))

; (listof Pair) (listof (listof (listof Pair)))
;     ->  (listof (listof (listof Pair)))
(define (add-to-non-overlapping-partition groups partitions)
  (if (empty? partitions)
      (list (list groups))
      (let-values ([(result added?)
                    (for/fold
                     ([result '()]
                      [added? #f])
                     ([current-partition partitions])
                      (if (or added? (not (exactly-single-overlaps? groups current-partition)))
                          (values (cons current-partition result) added?)
                          (values (cons (cons groups current-partition) result) #t)))])
        (if added? result (cons (list groups) result)))))

(define DEBUG (make-parameter #f))

(define (partition-groups groups-list)
  (for/fold ([partitions '()])  ;  (listof (listof (listof Pair)))
            ([current-groups groups-list]) ; current-groups : (listof Pair)
    (when (DEBUG) (displayln `(add-to-non-overlapping-partition ,current-groups ,partitions)))
    (add-to-non-overlapping-partition current-groups partitions)))


(define (remove-spurious groups-list)
  (argmax length (partition-groups groups-list)))
      




(struct fwf (e-mtx flat-mtx all-groups maj-groups final-groups data-frame))


(define (parse-fixed-width lines)
  (define e-mtx (fluff-e-matrix (lines->e-matrix lines)))
  (define lines-length (length lines))
  (define ll/2 (floor (/ lines-length 2)))
  (define ll/5 (floor (/ lines-length 5)))

  (define flat-mtx
    (append  
     (for/list ([n lines-length]) (matrix-flatten e-mtx n (max (- lines-length (* 2 n))
                                                               (- lines-length ll/5 n))))
     (for/list ([n lines-length]) (matrix-flatten e-mtx (min n ll/5) (max (- lines-length n) 0)))
     (for/list ([n ll/2]) (matrix-flatten e-mtx n ll/2))))

  (define all-groups (map decrement-all-values
                          (filter (λ(g) (< 1 (length g)))
                                  (map (lambda (flt) (extract-group-positions (e-vector->line flt)))
                                       flat-mtx))))

  (define all-group-counts (sort (map length all-groups) <))
  (define majority-count (car
                          (argmax cdr (build-list (add1 (apply max all-group-counts))
                                                  (λ(i) (cons i (length (filter (λ(n) (= n i))
                                                                                all-group-counts))))))))
  (define majority-groups (remove-spurious (filter (λ(g) (= (length g) majority-count)) all-groups)))

  (define final-groups (check&merge-groups (union-groups majority-groups) lines))

  (fwf e-mtx flat-mtx all-groups majority-groups final-groups #f))



(module+ test
  (check-expect (count-inconsistent-breaks '((0 . 3) (4 . 7) (8 . 13)) "abc def hijkl") 0)
  (check-expect (count-inconsistent-breaks '((0 . 3) (3 . 7) (7 . 13)) "abc def hijkl") 0)
  (check-expect (count-inconsistent-breaks '((0 . 3) (2 . 12) (7 . 13)) "abc def hijkl") 1)
  (check-expect (count-inconsistent-breaks '((0 . 2) (3 . 9) (7 . 13)) "abc def hijkl") 2 ))

(define (count-inconsistent-breaks groups txt)
  (for/sum ([pair groups])
    (if (and (space-at/before? (car pair) txt)
             (space-at/before? (cdr pair) txt))
        0 1)))


(module+ test
  (check-expect (space-at/before? 7 "abcde  fg") #t)
  (check-expect (space-at/before? 6 "abcde  fg") #t)
  (check-expect (space-at/before? 5 "abcde  fg") #t)
  (check-expect (space-at/before? 0 "abcde  fg") #t)  ; start/end string counts as space
  (check-expect (space-at/before? 9 "abcde  fg") #t)
  (check-expect (space-at/before? 53 "abcde  fg") #f) ; out of range
  (check-expect (space-at/before? 1 "abcde  fg") #f)
  (check-expect (space-at/before? 4 "abcde  fg") #f)
  (check-expect (space-at/before? 8 "abcde  fg") #f))

(define (space-at/before? pos txt)
  (and (<= 0 pos (string-length txt))
       (or (zero? pos)
           (= pos (string-length txt))
           (char-whitespace? (string-ref txt pos))
           (char-whitespace? (string-ref txt (sub1 pos))))))




(define (write-csv output-file-name fwf-parse data-lines)
  (write-file output-file-name
            (string-join 
             (for/list ;([line (take data-lines (min 5000 (length data-lines)))])
                 ([line data-lines])
               (string-join (map (λ(s) (string-append "\"" s "\""))
                                 ;(cons "-" (apply-groups (fwf-final-groups fwf-parse) line))
                                 (apply-groups (fwf-final-groups fwf-parse) line)) ","))
             "\n")))


(module+ test
  (check-expect (decrement-all-values '((3 . 4) (7 . 12)))
                '((2 . 3) (6 . 11))))

(define (decrement-all-values groups)
  (map (λ(g) (cons (sub1 (car g)) (sub1 (cdr g)))) groups))




(define (run-on-file input-file-name-or-port)
  (define DATA-1 (filter-blank (if (path-string? input-file-name-or-port)
                                   (port->lines (open-input-file input-file-name-or-port))
                                   (port->lines input-file-name-or-port))))
  (define SAMPLE-1 (take DATA-1 (min 100 (length DATA-1))))
  (define PARSE-1 (parse-fixed-width SAMPLE-1))

  #|
    (matrix->bitmap (lines->e-matrix SAMPLE-1))
    "^ no fluff"
    (matrix->bitmap (fwf-e-mtx PARSE-1))
    "^ fluff"
    (for/fold ([img (square 0 "solid" "white")])
              ([flt (fwf-flat-mtx PARSE-1)])
      (above img (matrix->bitmap (list flt))))
  |#

  (define CONSISTENT-ONLY
    (let ([threshold (quotient (length (fwf-final-groups PARSE-1)) 10)])
      (filter (λ(line) (<= (count-inconsistent-breaks (fwf-final-groups PARSE-1) line) threshold))
              DATA-1)))
  (define PARSED-LINES (map (λ(line) (map string-trim (apply-groups (fwf-final-groups PARSE-1) line)))
                       CONSISTENT-ONLY))
  
  ;(fwf-final-groups PARSE-1)
  ;(apply-groups (fwf-final-groups PARSE-1) (list-ref DATA-1 20))
  (write-csv "output.csv" PARSE-1 CONSISTENT-ONLY)
  
  (struct-copy fwf PARSE-1 [data-frame PARSED-LINES]))


(define R1 (run-on-file "est16-ga.txt"))
(define R2 (run-on-file "ssamatab2.txt"))

(module+ test
  (check-expect (length (fwf-final-groups R1)) 27)
  (check-expect (length (fwf-final-groups R2)) 10))

(define LINES1 (read-lines "est16-ga.txt"))
(define BREAKS1 (map (λ(line) (count-inconsistent-breaks (fwf-final-groups R1) line)) LINES1))
(define CONSISTENT-ONLY1
    (let ([threshold (quotient (length (fwf-final-groups R1)) 10)])
      (filter (λ(line) (<= (count-inconsistent-breaks (fwf-final-groups R1) line) threshold))
              LINES1)))
(define PARSED1 (map (λ(line) (map string-trim (apply-groups (fwf-final-groups R1) line)))
                     CONSISTENT-ONLY1))

(define LINES2 (read-lines "ssamatab2.txt"))
(define BREAKS2 (map (λ(line) (count-inconsistent-breaks (fwf-final-groups R2) line)) LINES2))


(module+ test (test))





;; TODO: (count-inconsistent-breaks FINAL (list-ref COMPLETE-LINES 20))
;;  count-inconsistent-breaks: [listof Pair]  string -> number

;; TODO: check on lots of fixed width data examples

