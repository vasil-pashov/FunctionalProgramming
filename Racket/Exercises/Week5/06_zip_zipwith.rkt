#lang racket
(define (zip-with f l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (f (car l1) (car l2)) (zip-with f (cdr l1) (cdr l2)))))

(define (zip l1 l2)
  (zip-with cons l1 l2))