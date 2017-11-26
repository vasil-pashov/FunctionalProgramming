#lang racket
(define (id f) f)

(define (compose . fns)
  (foldr (lambda(el acc)(lambda(x)(el (acc x)))) id fns))