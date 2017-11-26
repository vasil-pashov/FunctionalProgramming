#lang racket
(define (digits-list number)
  (define (digits-list-helper number rez)
    (if (= number 0)
        rez
        (digits-list-helper (quotient number 10) (cons (remainder number 10) rez))))
    (digits-list-helper number '()))