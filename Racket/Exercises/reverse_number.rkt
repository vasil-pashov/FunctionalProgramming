#lang racket
;Given a number reverse its digits
(define (reverse n)
  (define (reverse* n res)
    (if (= n 0)
        res
        (reverse* (quotient n 10) (+ (* 10 res) (remainder n 10)))
        ))
  (reverse* n 0))