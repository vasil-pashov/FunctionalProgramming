#lang racket
(define (bisection-method a b f eps)
  (define (helper a b iterations)
    (let ((mid-point (/ (+ a b) 2))
          (new-iter (1+ iterations)))
      (cond
        ((approx-zero? (- a b) eps) (cons mid-point iterations))
        ((negative? (* (f a) (f mid-point))) (helper a mid-point new-iter))
        (else (helper mid-point b new-iter)))))
  (helper a b 0))

(define (newton-method a b f eps)
  (define (next xi xi-1)
    (- xi (/ (f xi) (df/dx f xi eps))))
  (iterative-root-finder a b f next eps))

(define (secant-method a b f eps)
  (define (next xi xi-1)
    (let ((fxi (f xi)))
      (- xi (/(* fxi (- xi xi-1)) (- fxi (f xi-1))))))
  (iterative-root-finder a b f next eps))

; Generic implementation of iterative root finding
(define (iterative-root-finder a b f next eps)
  (define (helper xi xi-1 iterations)
    (if (approx-zero? (- xi xi-1) eps)
        (cons xi iterations)
        (helper (next xi xi-1) xi (+ 1 iterations))))
  (helper a b 0))

(define (find-root f a b eps)
  (car (newton-method a b f eps)))

;Pass methods to be compared as arguments
(define (compare-methods f a b eps m1 m2 . rest)
  (let ((to-cmp (append (list m1 m2) rest)))
    (map (lambda(method)(cdr (method a b f eps))) to-cmp)))


(define (df/dx f x delta-x)
  (/ (- (f (+ x delta-x)) (f x)) delta-x))

(define (approx-zero? x eps)
  (< (abs x) eps))

(define (1+ x)(+ 1 x))

;(define (secant-method a b f eps)
;  (define (helper xi xi-1 iterations)
;    (let ((diff (- xi xi-1))
;          (fxi (f xi)))
;      (if (approx-zero? diff eps)
;          (cons xi iterations)
;          (helper (- xi (/(* fxi diff) (- fxi (f xi-1)))) xi (1+ iterations)))))
;  (helper a b 0))

;(define (newton-method a b f eps)
;  (define (helper xi xi-1 iterations)
;    (if (approx-zero? (- xi xi-1) eps)
;        (cons xi iterations)
;        (helper (- xi (/ (f xi) (df/dx f xi eps))) xi (1+ iterations))))
;  (helper a b 0))