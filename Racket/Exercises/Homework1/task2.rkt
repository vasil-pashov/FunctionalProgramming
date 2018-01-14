#lang racket
(define (reduce n)
  (define (next n)
    (let ((max-digit (max-digit n)))
      (* max-digit (remove-leftmost max-digit n))))
  (define (helper n)
    (if (< n 10)
        n
        (helper (next n))))
  (helper n))

(define (max-digit n)
  (apply max (number->list n)))

(define (remove-leftmost digit n)
  (list->number (remove digit (number->list n))))

(define (number->list num)
  (define (last-digit n)
    (remainder n 10))
  (define (remove-last-digit n)
    (quotient n 10))
  (if (zero? num)
      '(0)
      (accumulate cons '() num 0 last-digit remove-last-digit =)))

(define (list->number lst)
  (define (append-digit digit num)
    (+ (* 10 num) digit))
  (foldl append-digit 0 lst))

(define (accumulate op nv a b term next end?)
  (define (helper a res)
    (if (end? a b)
        res
        (helper (next a) (op (term a) res))))
  (helper a nv))