#lang racket
(define (reduce current end acc operation next cond? term)
  (if (cond? current) acc
      (reduce (next current) end (operation (term current) acc) operation next cond? term)))
(define (sum a b)
  (reduce a b 0 + (lambda(x) (+ 1 x)) (lambda(x)(> x b)) (lambda(x) x)))
  