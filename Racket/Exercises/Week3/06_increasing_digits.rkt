#lang racket
;Check if the digits of a number are in increasing order
(define (increasing? n)
  (define (increasing-helper n prev)
    (define last (remainder n 10))
    (cond
      ([= n 0] #t)
      ([< prev last] #f)
      [(= prev last) #f]
      [else (increasing-helper (quotient n 10) last)]))
  (increasing-helper n 10))