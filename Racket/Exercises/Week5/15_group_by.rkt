#lang racket
(define (uniques l)
  (foldr (lambda(el acc)(if (member el acc) acc (cons el acc))) '() l))

(define (group-by f l)
  (map
   (lambda(rez)
     (cons rez
     (list (filter (lambda(x)(equal? rez (f x))) l))))
   (uniques (map f l)))
  )