#lang racket
;Find binomial coefficient using Pascal's triangle

(define (binomial row col)
  (cond
    ((= row col) 1)
    ((= col 1) 1)
    (else (+ (binomial (- row 1) (- col 1)) (binomial (- row 1) col)))))