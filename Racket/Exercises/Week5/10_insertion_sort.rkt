#lang racket
(define (insert-sorted new-el l)
  (cond
    ((null? l) (list new-el))
    ((< new-el (car l)) (cons new-el l))
    (else (cons (car l) (insert-sorted new-el (cdr l))))))

(define (insertion-sort l)
  (foldr insert-sorted '() l))