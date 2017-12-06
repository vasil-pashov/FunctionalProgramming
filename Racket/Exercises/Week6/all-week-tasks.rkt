#lang racket
;Checks if lst1 is prefix of lst2
(define (prefix? lst1 lst2)
 (let ((len (length lst1)))
   (and (<= len (length lst2))
        (equal? (take lst2 len) lst1))))

(define (sublist? lst1 lst2)
  (and lst2
       (or (prefix? lst1 lst2)
       (sublist? lst1 (member (car lst1) (cdr lst2))))))

(define (remove el lst)
  (filter (lambda(x)(not (equal? el x))) lst))

(define (uniques lst)
  (if (null? lst)
      '()
      (cons (car lst) (uniques (remove (car lst) (cdr lst))))))

(define (count el lst)
  (foldr (lambda(x res)(if (equal? x el) (+ res 1) res)) 0 lst))

(define (histogram lst)
  (let ((uniq (uniques lst)))
    (map cons uniq (map (lambda(x)(count x lst)) uniq))))


(define (transpose m)
  (apply map list m))

(define (transpose* m)
  (map (lambda(col)
         (map
          (lambda(row)(list-ref row col))
          m))(range (length (car m)))))

(define (all? p? lst)
  (= (length lst) (length (filter p? lst))))

(define (non-zero? x)(not (zero? x)))

(define (upper-triangular? m)
  (foldr (lambda(row-idx res)(and (all? zero? (take (list-ref m row-idx) row-idx)) res)) #t (range (length m))))

(define (main-diag m)
  (map (lambda(row-idx)(list-ref (list-ref m row-idx) row-idx)) (range (length m))))

(define remove-first-row cdr)
(define (remove-first-col m)
  (map cdr m))

(define (main-diag* m)
    (if (null? m)
        '()
        (cons (caar m) (main-diag* (remove-first-col (remove-first-row m))))))

(define (antidiag m)
  (reverse (main-diag (reverse m))))
