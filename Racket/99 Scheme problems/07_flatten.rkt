#lang racket
;Flatten a nested list
(define (flatten l)
  (define (flatten* l acc)
    (if (null? l)
        acc
        (let([head (car l)] [tail (cdr l)])
          (if (list? head)
              (flatten* tail (flatten* head acc))
              (flatten* tail (cons head acc))))))
  (reverse (flatten* l '())))
