#lang racket
(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t)))))

(define empty-stream? null?)
(define empty-stream '())
(define head car)
(define (tail stream) (force (cdr stream)))

(define (enum-stream a b)
  (if (> a b)
      empty-stream
      (cons-stream a (enum-stream (+ 1 a) b))))

(define (stream-take s n)
  (if (or (empty-stream? s) (= 0 n))
      '()
      (cons (head s) (stream-take (tail s) (- n 1)))))

(define (search-stream p? s)
  (cond
    ((empty-stream? s) #f)
    ((p? (head s)) s)
    (else (search-stream p? (tail s)))))

(define (from n)
  (cons-stream n (from (+ 1 n))))

(define nats (from 0))

(define (next-fib a b)
  (cons-stream a (next-fib b (+ a b))))
(define stream-fibs (next-fib 0 1))

(define (map-stream f stream)
  (cons-stream (f (head stream)) (map-stream f (tail stream))))

(define (filter-stream p? stream)
  (if (p? (head stream))
      (cons-stream (head stream) (filter-stream p? (tail stream)))
      (filter-stream p? (tail stream))))

(define (zip op s1 s2)
  (cons-stream (op (head s1) (head s2)) (zip op (tail s1) (tail s2))))
