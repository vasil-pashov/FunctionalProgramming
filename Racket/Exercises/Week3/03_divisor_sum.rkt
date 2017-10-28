#lang racket
;Divisor sum

(define (divisor_sum n)
  (define (divisor_sum_helper div sum)
    (cond
      [(= div 1) sum]
      [(= (remainder n div) 0) (divisor_sum_helper (- div 1) (+ sum div))]
      [else (divisor_sum_helper (- div 1) sum)]))
  (divisor_sum_helper n 1))

;accumulator function
(define (accumulate operation from to null_value terminate? next term)
  (define (accumulate_helper i accumulator)
    (if (terminate? i to)
      accumulator
      (accumulate_helper (next i) (operation (term i) accumulator))))
  (accumulate_helper from null_value))

;Divisor sum using accumulating function
(define (divisor_sum1 n)
  (accumulate + 1 n 0 (lambda(a b)(> a b)) (lambda(x) (+ x 1)) (lambda(x)(if (= (remainder n x) 0) x 0)))
  )