#lang racket

(define (all? p? lst)
  (foldr (lambda(el acc)(and (p? el) acc)) #t lst))

(define (matrix? m)
  (and
   (list? m)
   (not (null? m))
   (not (null? (car m)))
   (all? list? m)
   (all? (lambda(el)(= (length el) (length (car m)))) (cdr m))))

(define (delete idx lst)
  (append (take lst idx) (drop lst (+ idx 1))))

(define matrix-rows length)

(define (matrix-cols m)
  (length (car m)))

(define first-row car)

(define (first-col m)
  (map car m))

(define (ith-row i m)
  (list-ref m i))

(define (jth-col j m)
  (map (lambda(row)(list-ref row j)) m))

(define remove-ith-row delete)

(define (remove-jth-col j m)
  (map (lambda(row) (delete j row)) m))

(define (transpose m)
  (apply map list m))

(define (add a b)
  (define (vector-sum v1 v2)
    (map + v1 v2))
  (map vector-sum a b))

(define (multiply a b)
  (define (dot-product v1 v2)
    (apply + (map * v1 v2)))
  (let ((t (transpose b)))
    (map (lambda(row)
           (map (lambda(col) (dot-product row col)) t)) a)))




