#lang racket
;Find the last box of a list

(define (last list)
  (cond
    [(eq? list '()) '()]
    [(eq? (cdr list) '()) (car list)]
    [else (last (cdr list))]))