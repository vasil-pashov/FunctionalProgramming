#lang racket
;Find the K-th element of a list

(define (k-th list index)
    (cond
      [(eq? list '()) "Error"]
      [(= index 1) (car list)]
      [else (k-th (cdr list) (- index 1))]))