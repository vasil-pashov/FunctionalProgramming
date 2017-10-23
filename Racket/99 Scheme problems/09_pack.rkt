#lang racket
;Pack consecutive duplicates of list elements into sublists

(define (pack l)
  (define (pack_sequence l acc current)
    (cond
      [(null? l) (cons acc '())]
      [(equal? (car l) current) (pack_sequence (cdr l) (cons (car l) acc) current)]
      [(cons acc l)]
    ))
  (define (pack* l acc)
    (if (null? l)
        acc
        (let ([packed (pack_sequence l '() (car l))])
          (pack* (cdr packed) (cons (car packed) acc)))))
  (reverse (pack* l '())))