#lang racket
(define (accumulate op from to null-val term next)
  (define (accumulate-helper current acc)
    (if (> current to)
        acc
        (accumulate-helper (next current) (op acc (term current)))))
  (accumulate-helper from null-val))

(define (1+ a) (+ a 1))

(define (count predicate? a b)
  (accumulate + a b 0 (lambda(i)(if (predicate? i) 1 0)) 1+))

(define (exists? predicate? a b)
  (accumulate (lambda(i j)(or i j)) a b #f predicate? 1+))

(define (for-all? predicate? a b)
  (accumulate (lambda(i j)(and i j)) a b #t predicate? 1+))