#lang racket

;Generate first cnt prime numbers

(define (is_prime? n)
  (define (has_divisor? end)
  (cond
    [(= end 1) #f]
    [(= (remainder n end) 0) #t]
    [else (has_divisor? (- end 1))]
    ))
  (not (has_divisor? (round (sqrt n)))))

(define (primes cnt)
  (define (primes* n l)
    (if [= (length l) cnt]
        l
        {if (is_prime? n)
            (primes* (+ n 1) (append l (cons n '())))
            (primes* (+ n 1) l)}))
  (primes* 3 '(2)))