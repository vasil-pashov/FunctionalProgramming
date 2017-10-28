#lang racket

(define (dec-to-binary n)
  (if (= n 0)
      0
      (+ (remainder n 2) (* 10 (dec-to-binary (quotient n 2))))))

(define (accumulate operation from to null_value terminate? next term)
  (define (accumulate_helper i accumulator)
    (if (terminate? i to)
      accumulator
      (accumulate_helper (next i) (operation (term i) accumulator))))
  (accumulate_helper from null_value))

(define (binary-to-dec n)
  (define (next i) (list (quotient (car i) 10) (* (cadr i) 2)))
  (define (term i) (* (remainder (car i) 10) (cadr i)))
  (accumulate + (list n 1) 0 0 (lambda(i end)(= (car i) end)) next term))