#lang racket
;Count the atoms in arbitrary nested list
(define (atom? el) (and (not (null? el)) (not (pair? el))))

(define (count-atoms l)
  (cond
    ((null? l) 0)
    ((atom? l) 1)
    (else (+ (count-atoms (car l)) (count-atoms (cdr l))))
   ))

(define (flatten l)
  (cond
    ((null? l) '())
    ((atom? l) (list l))
    (else (append (flatten (car l)) (flatten (cdr l))))
    ))

(define (deep-reverse l)
  (cond
    ((null? l) '())
    ((atom? l) l)
    (else
     (append (deep-reverse (cdr l)) (list (deep-reverse (car l))))
     )
   )
  )

(define (deep-fold hnull term op l)
  (cond
    ((null? l) hnull)
    ((atom? l) (term l))
    (else (op (deep-fold hnull term op (car l)) (deep-fold hnull term op (cdr l))))
   )
  )