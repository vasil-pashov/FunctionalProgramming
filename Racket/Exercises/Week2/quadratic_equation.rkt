#lang racket
(define (quadratic_equation a b c)
  (define D (- (* b b) (* (* 4 a) c)))
  (if (= a 0)
      (if (and (= b 0) (= c 0))
          "Every x"
          (if (= b 0)
              "No solution"
              (/ (- c) b)))
      (let ([sqrtD (sqrt D)])
        (cond
          [(< D 0) "No real roots"]
          [(= sqrtD 0) (/ (- b) (* 2 a))]
          [else (cons (/ (- (- b) sqrtD) (* 2 a)) (/ (+ (- b) sqrtD) (* 2 a)))]
          )
        )
      )
  )