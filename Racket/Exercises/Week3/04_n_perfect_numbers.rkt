#lang racket
;Perfect numbers -> sum of all divisors except the number equals the number

;is number prefect
(define (perfect? n)
  (define (perfect_helper i acc)
    (let ([rem (remainder n i)])
      (cond
        [(= n i) (= acc n)]
        [(> acc n) #f]
        [(= rem 0) (perfect_helper (+ 1 i) (+ i acc))]
        [else (perfect_helper (+ 1 i) acc)])))
  (perfect_helper 1 0))

;return first n perfect numbers
(define (n_perfects n)
  (define (n_perfects_helper i l)
    (cond
      [(= (length l) n) l]
      [(perfect? i) (n_perfects_helper (+ 1 i) (cons i l))]
      [else (n_perfects_helper (+ 1 i) l)]))
  (n_perfects_helper 1 '()))