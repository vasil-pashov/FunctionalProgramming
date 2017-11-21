;n-th element of list, returnst false if n > length of list
(define (nth n l)
  (cond
    ((null? l) #f)
    ((< n 0) #f)
    ((= 0 n) (car l))
    (else (nth (- n 1) (cdr l)))))