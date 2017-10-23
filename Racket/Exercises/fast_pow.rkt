#lang racket
;Fast raise a number to a power

(define (power n pow)
  (define (square a) (* a a))
  (define original n)
  (cond
    [(= pow 1) n]
    [(= pow 0) 1]
    [(even? pow) (power (square n) (quotient pow 2))]
    [else (* original (power n (- pow 1)))]
    )
  )