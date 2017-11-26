#lang racket
(define (uniques l)
  (foldr (lambda(el acc)(if (member el acc) acc (cons el acc))) '() l))