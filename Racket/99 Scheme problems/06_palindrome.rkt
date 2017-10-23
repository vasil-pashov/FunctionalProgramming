#lang racket
(define (palindrome? list)
  (equal? list (reverse list)))