#lang racket
(define (reverse-int n)
  (define (reverse-int-helper i acc)
    (if (= i 0)
        acc
        (reverse-int-helper (quotient i 10) (+ (* 10 acc) (remainder i 10)))))
  (reverse-int-helper n 0))

(define (palindrome? n)
  (= n (reverse-int n)))