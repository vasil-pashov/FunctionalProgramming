;Return list length
(define (len l)
  (define (length-help list length)
    (if (null? list)
        length
        (length-help (cdr list) (+ 1 length))))
  (length-help l 0))

;Reverse list
(define (rev l)
  (define (rev-help list reversed)
    (if (null? list)
        reversed
        (rev-help (cdr list) (cons (car list) reversed))))
  (rev-help l '()))

;Custom map function
(define (my-map f list)
    (if (null? list)
        '()
        (cons (f (car list)) (my-map f (cdr list)))))

;Custom filter
(define (my-filter p? l)
  (if (null? l)
      '()
      (let ((head (car l)))
        (if (p? head)
            (cons head (my-filter p? (cdr l)))
            (my-filter p? (cdr l))))))