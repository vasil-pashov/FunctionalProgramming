#lang racket

;Check where does a point lie in a Decart coordinate system

(define (quadrant x y)
  (cond
    ([= x 0]
      (if [= y 0]
          "Origin"
          "Oy"
          ))
    ([= y 0] "Ox")
    ([< x 0] (if [< y 0] "III" "II"))
    (else (if [< y 0] "IV" "I"))))

