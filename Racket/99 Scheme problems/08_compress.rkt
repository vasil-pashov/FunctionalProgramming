#lang racket
;Eliminate consecutive duplicates of list elements

(define (eliminate_duplicates l)
  (define (eliminate_duplicates* l acc)
    (if (null? l)
        acc
        (let ([head_l (car l)][head_acc (car acc)][tail (cdr l)])
          (if (equal? head_l head_acc)
              (eliminate_duplicates* tail acc)
              (eliminate_duplicates* tail (cons head_l acc))))))
    (reverse (eliminate_duplicates* (cdr l) (list (car l)))))
