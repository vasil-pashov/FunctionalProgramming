#lang racket

(define (quick-sort lst)
  (if (null? lst)
      '()
      (let ((head (car lst)) (tail (cdr lst)))
        (append (quick-sort (filter (lambda(x)(<= x head)) tail))
                (list head)
                (quick-sort (filter (lambda(x)(> x head)) tail))))))