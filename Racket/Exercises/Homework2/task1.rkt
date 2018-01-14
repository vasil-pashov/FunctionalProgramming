#lang racket
(define(row-reduce m)
  (let* ((pivot-row-idx (pivot-row m))
         (pivot-row (list-ref m pivot-row-idx)))
    (append
     (eliminate pivot-row (take m pivot-row-idx))
     (list pivot-row)
     (eliminate pivot-row (drop m (+ 1 pivot-row-idx))))))
  
(define (eliminate row m)
  (map (lambda(r) (add-vectors (scalar-mult row (- (/ (car r) (car row)))) r)) m))

(define (add-vectors r1 r2)
  (map + r1 r2))

(define (scalar-mult v c)
  (map (lambda(x) (* c x)) v))

(define (pivot-row m)
  (define (helper m idx)
    (cond
      ((null? m) #f)
      ((null? (car m)) #f)
      ((not (zero? (caar m))) idx)
      (else (helper (cdr m) (+ idx 1)))))
  (helper m 0))