#lang racket

;solve is prime number uing accumulator function

(define (accumulate operation from to null_value terminate? next term)
  (define (accumulate_helper i accumulator)
    (if (terminate? i to)
      accumulator
      (accumulate_helper (next i) (operation (term i) accumulator))))
  (accumulate_helper from null_value))

(define (prime? n)
  (define (my-and a b) (and a b))
  (define to (floor (sqrt n)))
  (accumulate my-and 2 to #t (lambda(i end) (> i end)) (lambda(i)(+ i 1)) (lambda(i) (not (= (remainder n i) 0)))))

(define (n-primes n)
  (define (n-primes-helper it res)
    (cond
      [(= (length res) n) res]
      [(prime? it) (n-primes-helper (+ 1 it) (cons it res))]
      [else (n-primes-helper (+ 1 it) res)]))
  (n-primes-helper 1 '()))