#lang racket
(define (my-arity . args)
  (length args))