#lang racket
(define (fib n)
	(define (fib* n x1 x2)
		(if (= n 1)
			x2
			(fib* [- n 1] x2 [+ x1 x2])
			))
	(fib* n 0 1))