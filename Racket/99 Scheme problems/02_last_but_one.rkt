#lang racket
;Find the last but one element of a list

(define (last-but-one list)
  (cond
    [(or (null? list) (null? (cdr list))) "Error"]
    [(null? (cddr list)) (car list)]
    [else (last-but-one (cdr list))]
    )
  )