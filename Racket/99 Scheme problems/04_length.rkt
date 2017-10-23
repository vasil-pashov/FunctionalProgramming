#lang racket
;Find the number of elements of a list.

(define (len-slow list)
  (if (null? list) 0 (+ 1 (len-slow (cdr list)))))

(define (len list)
  (define (len* list acc)
    (if (null? list) acc (len* (cdr list) (+ 1 acc))))
    (len* list 0)
  )