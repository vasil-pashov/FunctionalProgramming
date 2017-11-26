#lang racket

(define (merge a b)
  (cond
    ((null? a) b)
    ((null? b) a)
    ((< (car a) (car b)) (cons (car a) (merge (cdr a) b)))
    (else (cons (car b) (merge a (cdr b))))))

(define (merge-sort lst)
  (if (< (length lst) 2)
      lst
   (merge (merge-sort (take lst (quotient (length lst) 2)))
          (merge-sort (drop lst (quotient (length lst) 2))))))