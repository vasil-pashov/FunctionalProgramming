#lang racket
(define (all? p? list)
  (foldr (lambda(el acc)(and (p? el) acc)) #t list))

(define (any? p? list)
  (foldr (lambda(el acc)(or (p? el) acc)) #f list))