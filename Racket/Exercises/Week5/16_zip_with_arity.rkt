#lang racket
(define (zip-with op . args)
  (if (or (member '() args) (null? args))
      '()
      (cons (apply op (map car args)) (apply zip-with op (map cdr args)))))