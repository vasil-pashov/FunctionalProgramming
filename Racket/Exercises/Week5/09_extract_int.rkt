#lang racket
(define (extract-ints l)
  (filter integer? l))