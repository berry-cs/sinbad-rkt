#lang racket

(require racket/dict
         math/distributions)
 
;; Divides the set of points into k clusters
;; using the standard k-means clustering algorithm
(define (k-means data k #:initialization (init k-means++))
  (define (iteration centroids)     
    (map centroid (clusterize data centroids)))
  (fixed-point iteration (init data k) #:same-test small-shift?))
 
;; Finds the centroid for a set of points
(define (centroid pts)
  (vector-map (curryr / (length pts))
       (for/fold ([sum (car pts)]) ([x (in-list (cdr pts))])
         (vector-map + x sum))))
 
;; Divides the set of points into clusters
;; using given centroids
(define (clusterize data centroids)
  (for*/fold ([res (map list centroids)]) ([x (in-list data)])
    (define c (argmin (distance-to x) centroids))
    (dict-set res c (cons x (dict-ref res c)))))
 
;; Stop criterion: all centroids change their positions
;; by less then 0.1% of the minimal distance between centroids.
(define (small-shift? c1 c2)
  (define min-distance
    (apply min
           (for*/list ([x (in-list c2)]
                       [y (in-list c2)] #:unless (equal? x y))
             ((metric) x y))))
  (for/and ([a (in-list c1)] [b (in-list c2)])
    (< ((metric) a b) (* 0.001 min-distance))))



(define (fixed-point f x0 #:same-test [same? equal?])
  (let loop ([x x0] [fx (f x0)])
    (if (same? x fx) fx (loop fx (f fx)))))
 



;; picks k points from a dataset randomly
(define (random-choice data k)
  (for/list ([i (in-range k)])
    (list-ref data (random (length data)))))
 
;; uses k-means++ algorithm
(define (k-means++ data k)
  (for/fold ([centroids (random-choice data 1)]) ([i (in-range (- k 1))])
    (define weights
      (for/list ([x (in-list data)])
        (apply min (map (distance-to x) centroids))))
    (define new-centroid
      (sample (discrete-dist data weights)))
    (cons new-centroid centroids)))





(define (euclidean-distance a b)
  (for/sum ([x (in-vector a)] [y (in-vector b)]) 
    (sqr (- x y))))
 
(define (manhattan-distance a b)
  (for/sum ([x (in-vector a)] [y (in-vector b)]) 
    (abs (- x y))))
 
(define metric (make-parameter euclidean-distance))
(define (distance-to x) (curry (metric) x))


;(define (spaces-difference-only a b)
;  (for/sum ([y (in-vector b)])
;    y))
;(metric spaces-difference-only)




(define (gaussian-cluster N 
                          #:stdev (σ 1) 
                          #:center (r0 #(0 0)) 
                          #:dim (d 2))
  (for/list ([i (in-range N)])
    (define r (for/vector ([j (in-range d)]) (sample (normal-dist 0 σ))))
    (vector-map + r r0)))
 
(define (uniform-cluster N 
                         #:radius (R 1) 
                         #:center (r0 #(0 0)))
  (for/list ([i (in-range N)])
    (define r (* R (sqrt (sample (uniform-dist)))))
    (define φ (* 2 pi (sample (uniform-dist))))
    (vector-map + r0 (vector (* r (cos φ)) (* r (sin φ))))))




(require plot)
 
(define (show-clustering data k #:method (method k-means++))
  (define c (k-means data k #:initialization method))
  (display
   (plot 
    (append
     (for/list ([d (clusterize data c)] 
                [i (in-naturals)])
       (points d #:color i #:sym 'fullcircle1))
     (list (points c  
                   #:sym 'fullcircle7
                   #:fill-color 'yellow
                   #:line-width 3)))
    #:title (format "Initializing by ~a" (object-name method)))))




(define DATA (map vector (list 1 2 3  7 8 9   20 23 22   70 71 75)))
(k-means DATA 5)



#;
(module+ test
  (define circle (uniform-cluster 10000))
  ; using k-means++ method
  (show-clustering circle 6)
  ; using standard k-means method
  (show-clustering circle 6 #:method random-choice)
  ; using manhattan distance
  (parameterize ([metric manhattan-distance])
    (show-clustering circle 6)))



(require 2htdp/batch-io)
(define LINES (read-lines "cols-header.txt"))




;; produce
;  (spaces-before 9 "ab  d  fff   gh") ==>   4
(define (spaces-before col line)
  (for/sum ([c (substring line 0 (min (string-length line) col))])
    (if (char=? #\space c) 1 0)))


(define (avg-spaces-before col lines)
  (define sum-spaces (for/sum ([line lines]) (spaces-before col line)))
  (/ sum-spaces (length lines)))

(avg-spaces-before 5 (list "ab cd" "ab cd" "a bcd"))



(define (col-space col line)
  (if (and (< col (string-length line))
           (char-whitespace? (string-ref line col)))
      1
      0))



(define (consecutive-spaces-before col line)
  (string-length
   (first (regexp-match #px"\\s*$"
                        (substring line 0 (min (string-length line) col))))))

(= (consecutive-spaces-before 9 "ab  d  fff   gh") 0)
(= (consecutive-spaces-before 4 "ab  d  fff   gh") 2)

(define (consecutive-spaces-after col line)
  (string-length
   (first (regexp-match #px"^\\s*"
                        (substring line (min (string-length line) col))))))

(= (consecutive-spaces-after 9 "ab  d  fff   gh") 0)
(= (consecutive-spaces-after 4 "ab  d  fff   gh") 0)
(= (consecutive-spaces-after 5 "ab  d  fff   gh") 2)
(= (consecutive-spaces-after 10 "ab  d  fff   gh") 3)



(define (avg-over-lines F col lines)
  (define sum-stats (for/sum ([line lines]) (F col line)))
  (/ sum-stats (length lines)))





(define TEST-DATA
  (string-split
#|        111111111122222222223 
0123456789012345678901234567890 |# "
Here's most data that you got
Dates     145.67 fa     A  45.0
Oranges     3.99 spr        1.4
Bananas     0.99 yearly Q    .5
Kiwi      312.00 sum    Z  14.3
Apples      1.50 sum    R 675.1" "\n"))
; 0, 8, 15, 20






;  [listof (listof Number)) (listof Number -> Number) Number -> (listof (listof Number))
(define (peaks a-lop sample-size pos-func [threshold .1])
  (cons (first a-lop)
        (filter (λ(p)
                  (define index (second p))
                  (define prev (list-ref a-lop (sub1 index)))
                  (define next (list-ref a-lop (add1 index)))
                  (and (> (pos-func p) (/ sample-size 2))
                       (> (pos-func p) (* (+ 1 threshold) (pos-func prev)))
                       (> (pos-func p) (* (+ 1 threshold) (pos-func next)))))
                (rest (take a-lop (sub1 (length a-lop)))))))

(define TABLE
  (let ([TEST-DATA LINES])
    (for/list [(col (in-range (add1 (apply max (map string-length TEST-DATA)))))]
      (list
       (if (< col (string-length (second TEST-DATA)))
           (substring (second TEST-DATA) col (add1 col))
           "")
       col
       (* (length TEST-DATA) (avg-over-lines col-space col TEST-DATA))
       (* (length TEST-DATA) (avg-over-lines consecutive-spaces-before col TEST-DATA))
       (* (length TEST-DATA) (avg-over-lines consecutive-spaces-after col TEST-DATA))))))

#|
TABLE
(peaks TABLE 20 fourth)
(peaks TABLE 20 fifth)
(peaks TABLE 20 third)
|#





#|

 y[i] =  (consecutive-spaces-before (i) * multiplier) if y[i-1] space and y[i] not space
        =  y[i-1] + 10 if y[i-1] non space and y[i] space
        =  y[i-1] if y[i-1]&y[i] space or both non-space

|#

; string -> vector[number]
(define (column-weights line max-length)
  (define line-length (string-length line))
  (for/fold ([wgts (make-vector max-length)])
            ([col max-length])  ; col is the i+1 above
    (define wgt_col-1 (if (zero? col) 0 (vector-ref wgts (- col 1))))
    (define space-at
      (and (< col line-length) (char-whitespace? (string-ref line col))))
    (define space-before
      (and (< -1 (sub1 col) line-length) (char-whitespace? (string-ref line (sub1 col)))))

    (define wgt_col
      (match (list space-before space-at)
        ['(#f #f) wgt_col-1]
        ['(#t #t) wgt_col-1]
        ['(#f #t) (+ 1 wgt_col-1)]
        ['(#t #f) (+ wgt_col-1 (consecutive-spaces-before col line))]))

    (vector-set! wgts col wgt_col)    
    wgts))

"ab  d  fff   gh"
(column-weights "ab  d  fff   gh" 20)

"ab  dg fffqj gh"
(column-weights "ab  dg fffqj gh" 20)





(define max-line-length (add1 (apply max (map string-length LINES))))

(define COL-DATA
  (for/list ([col max-line-length])
    (vector col (* 10 (avg-spaces-before col LINES)))))

(define NUMBER-OF-CLUSTERS 10)

(sort (map (λ(v) (floor (vector-ref v 0)))
           (k-means COL-DATA NUMBER-OF-CLUSTERS))
      <)


(define CLUSTERS
  (sort
   (map
    (λ(vs)
      (define cols (map (λ(v) (floor (vector-ref v 0))) (rest vs)))
      (list (apply min cols) (apply max cols)))
    (clusterize COL-DATA (k-means COL-DATA NUMBER-OF-CLUSTERS)))
   (λ (p1 p2)
     (< (first p1) (first p2)))))
CLUSTERS

;(define LINE "MT1233100000000     12       33100      Miami-Fort Lauderdale-West Palm Beach, FL MSA                    1990    01     2,067,452         1,935,856       131,596           6.4")
;(map (λ (p) (substring LINE (first p) (second p))) CLUSTERS)



