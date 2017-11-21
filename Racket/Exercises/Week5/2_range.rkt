;range from a to b inclusive
;using recursion
(define (range from to)
  (define (range-help current)
    (if (> current to)
        '()
        (cons current (range-help (+ current 1)))))
    (range-help from))