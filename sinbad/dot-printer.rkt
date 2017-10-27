#lang racket

(provide dot-printer
         stop-dot-printer
         dot-printer-enabled)

(define dot-printer-enabled (make-parameter #t (lambda (v) (equal? v #t))))

(define dot-printer-max-delay       (make-parameter 2))     ; seconds
(define dot-printer-delay-increment (make-parameter .25))   ; seconds
(define dot-printer-initial-delay   (make-parameter .5))





(struct dp (delay first-msg first-msg-delay first-msg-printed thread) #:mutable #:transparent)

(define (dot-printer first-msg [first-msg-delay (dot-printer-max-delay)])
  (define D
    (dp (dot-printer-initial-delay)
        first-msg
        first-msg-delay
        #f
        #f))

  (define T
    (thread (lambda () (let LOOP () (when (dot-printer-enabled)
                                      (run-dot-printer D)
                                      (LOOP))))))
  (set-dp-thread! D T)
  D)


(define (run-dot-printer d)
  (when (not (dp-first-msg-printed d))
    (sleep (dp-first-msg-delay d))
    (printf "~a~n" (dp-first-msg d))
    (flush-output))

  (set-dp-first-msg-printed! d #t)

  (printf ".") (flush-output)
  (sleep (dp-delay d))
  (when (< (dp-delay d) (dot-printer-max-delay))
    (set-dp-delay! d (min (dot-printer-max-delay)
                          (+ (dp-delay d) (dot-printer-delay-increment)))))
  d)


(define (stop-dot-printer d)
  (when (dp-first-msg-printed d)
    (printf "Done.\n") (flush-output))
  (when (dp-thread d)
    (kill-thread (dp-thread d))))