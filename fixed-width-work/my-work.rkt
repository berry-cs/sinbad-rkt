#lang racket

(module+ test
  (require test-engine/racket-tests))


(require 2htdp/batch-io)


(module+ test
  (check-expect (filter-blank (list "  " "   fa   " "afeji" "\t   \t" "gg"))
                (list "   fa   " "afeji" "gg")))

(define (filter-blank a-los)
  (filter (λ(s) (not (regexp-match-exact? #px"\\s*" s))) a-los))


(define COMPLETE-LINES (filter-blank (read-lines "est16-ga.txt")))
;(define COMPLETE-LINES (filter-blank (read-lines "cedata.txt")))
;(define COMPLETE-LINES (filter-blank (read-lines "exh4s.txt")))
;(define COMPLETE-LINES (filter-blank (read-lines "cols-header.txt")))

(define SAMPLE-LINES (take COMPLETE-LINES (min 100 (length COMPLETE-LINES))))
(define LINES SAMPLE-LINES)


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
      ;(displayln `(check ,row-index ,col-index))
      (define cell (vector-ref row col-index))
      (when (and (zero? cell)
                 (< 0 row-index (sub1 (vector-length mtx)))
                 (< 0 col-index (sub1 (vector-length row))))
        (when (<= 3 (count-one-nbrs mtx row-index col-index))
          (vector-set! row col-index 2))
        ;(when (inter-phrase-space mtx row-index col-index)
        ;  (vector-set! row col-index 3))
        )))
                     ;(add1 (smallest-non-zero-nbr mtx row-index col-index))))
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


(require (only-in 2htdp/image
                  beside above square))

(define (matrix->bitmap mtx)
    (for/fold ([img (square 0 "solid" "white")])
              ([row mtx])
      (above
       img
       (for/fold ([row-img (square 0 "solid" "white")])
                 ([cell row])
         (beside row-img
                 (square 5 "solid" (match cell
                                     [0 "white"]
                                     [1 "black"]
                                     [2 "darkslategray"]
                                     [_ "blue"])))))))


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

(define (majority-candidate seq)
  (define-values (result count)
    (for/fold ([maj-elt (first seq)]
               [maj-count 1])
              ([elt (rest seq)])
      (cond
        [(= elt maj-elt) (values maj-elt (add1 maj-count))]
        [(= maj-count 1) (values elt 1)]
        [else (values maj-elt (sub1 maj-count))])))
  result)

(define (check-majority-candidate seq val)
  (> (for/sum ([elt seq]) (if (= elt val) 1 0))
     (/ (sequence-length seq) 2)))

(define (majority-element seq)
  (define candidate (majority-candidate seq))
  (and (check-majority-candidate seq candidate)
       candidate))


(module+ test
  (check-expect (majority-element '(2 3 3 4 5 3 3 3 7 3 5)) 3)
  (check-expect (majority-element '(2 3 3 4 5 3 8 4 7 3 5)) #f))


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
             (min (sub1 (car group)) (string-length line))
             (min (sub1 (cdr group)) (string-length line))) )

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
      




(define LINES-E-MATRIX+FLUFF (fluff-e-matrix (lines->e-matrix LINES)))

(matrix->bitmap (lines->e-matrix LINES))
"^ no fluff"
(matrix->bitmap LINES-E-MATRIX+FLUFF)
"^ fluff"

"x"

#|
(define TOP-DOWN-FLATTENS
  (for/list ([n (length LINES)]) (matrix-flatten LINES-E-MATRIX+FLUFF n)))

(define BOTTOM-UP-FLATTENS
  (for/list ([n (length LINES)])
    (matrix-flatten LINES-E-MATRIX+FLUFF (- (length LINES) n))))
|#

(define LINES-LENGTH (length LINES))
(define LINES-LENGTH/2 (floor (/ LINES-LENGTH 2)))
(define LINES-LENGTH/5 (floor (/ LINES-LENGTH 5)))  

(define FLATTENS
  (append 
   (for/list ([n LINES-LENGTH])
     (matrix-flatten LINES-E-MATRIX+FLUFF n (if (< n LINES-LENGTH/5)
                                                (- LINES-LENGTH (* 2 n))
                                                (- LINES-LENGTH LINES-LENGTH/5 n))))
   
   (list (make-vector (vector-length (vector-ref LINES-E-MATRIX+FLUFF 0))))

   (for/list ([n LINES-LENGTH])
     (matrix-flatten LINES-E-MATRIX+FLUFF (min n LINES-LENGTH/5) (max (- LINES-LENGTH n) 0)))
   (list (make-vector (vector-length (vector-ref LINES-E-MATRIX+FLUFF 0))))
   
   (for/list ([n LINES-LENGTH/2]) (matrix-flatten LINES-E-MATRIX+FLUFF n LINES-LENGTH/2))
   (list (make-vector (vector-length (vector-ref LINES-E-MATRIX+FLUFF 0))))
   
   ))


;(append TOP-DOWN-FLATTENS (list (make-vector (vector-length (first TOP-DOWN-FLATTENS)))) BOTTOM-UP-FLATTENS)


(for/fold ([img (square 0 "solid" "white")])
          ([flt FLATTENS])
  (above img (matrix->bitmap (list flt))))


(define GROUPS
  (filter (λ(g) (< 1 (length g)))
          (map (lambda (flt) (extract-group-positions (e-vector->line flt))) FLATTENS)))

(define GROUP-COUNTS
  (sort (map length GROUPS) <))

(define MAJORITY-COUNT
  (car (argmax cdr
               (build-list (add1 (apply max GROUP-COUNTS))
                           (λ(i) (cons i (length (filter (λ(n) (= n i)) GROUP-COUNTS))))))))

;(define MAJORITY-COUNT (majority-element GROUP-COUNTS))

(define MAJ-GROUPS
  (remove-spurious (filter (λ(g) (= (length g) MAJORITY-COUNT)) GROUPS)))

(define UNIONED-MAJ (union-groups MAJ-GROUPS))
UNIONED-MAJ

(apply-groups UNIONED-MAJ (list-ref COMPLETE-LINES 20))
(define FINAL (check&merge-groups UNIONED-MAJ SAMPLE-LINES))
FINAL

(apply-groups FINAL (list-ref COMPLETE-LINES 20))


(write-file "output.csv"
            (string-join 
             (for/list ([line (take COMPLETE-LINES (min 5000 (length COMPLETE-LINES)))])
               (string-join (map (λ(s) (string-append "\"" s "\""))
                                 (cons "extra" (apply-groups FINAL line))) ","))
             "\n"))


(module+ test (test))



;; TODO: (count-inconsistent-breaks FINAL (list-ref COMPLETE-LINES 20))
;;  count-inconsistent-breaks: [listof Pair]  string -> number

;; TODO: check on lots of fixed width data examples

