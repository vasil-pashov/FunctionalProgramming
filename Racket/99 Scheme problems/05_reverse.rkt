#lang racket

;Reverse a list

(define (my-reverse list)
  (define (my-reverse* list acc)
    (if (null? list) acc (my-reverse* (cdr list) (cons (car list) acc))))
  (my-reverse* list '()))