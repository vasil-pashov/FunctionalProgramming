(define (take n list)
  (cond
    ((<= n 0) '())
    ((null? list) '())
    (else (cons (car list) (take (- n 1) (cdr list))))))

(define (drop n list)
  (cond
    ((<= n 0) list)
    ((null? list) '())
    (else (drop (- n 1) (cdr list)))))