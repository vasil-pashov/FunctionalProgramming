#lang racket

(define (split l index)
  (define (split* l index front)
    (if (= index 1) (list front (cdr l) (car l)) (split* (cdr l) (- index 1) (append  front (list (car l))))))
  (split* l index '()))

(define (swap l index1 index2)
  (define h (split l index1))
  (define t (split (cadr h) (- index2 index1)))
  (append (car h) (list (caddr t)) (car t) (list (caddr h)) (cadr t))
  )