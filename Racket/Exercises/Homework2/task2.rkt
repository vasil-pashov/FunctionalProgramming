#lang racket
(define (cross-out m)
  (define (cross i j m)
    (remove-col j (remove-row i m)))
  (foldr (lambda(i res)
               (append res (foldr
                 (lambda(j res)(cons (cross i j m) res))
                 '() (range (matrix-cols m)))))
         '() (range (matrix-rows m))))

(define matrix-rows length)
(define (matrix-cols m) (length (car m)))
(define (remove-row idx m)
  (append (take m idx) (drop m (+ 1 idx))))
(define (remove-col idx m)
  (map (lambda(row)(del-nth idx row))  m))
(define (del-nth n lst)
  (append (take lst n) (drop lst (+ 1 n))))