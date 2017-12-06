#lang racket

(provide new-assoc-list
         empty-assoc?
         del-assoc
         add-assoc
         keys
         values
         get-assoc)

(define new-assoc-list '())
(define empty-assoc? null?)

(define (make-assoc-list keys f)
  (map (lambda(x)(cons x (f x)))  keys))

(define (del-assoc key assoc-list)
  (filter (lambda(assoc)(not (equal? key (car assoc)))) assoc-list))

(define (add-assoc key value assoc-list)
  (cons (cons key value) (del-assoc key assoc-list)))

(define (keys assoc-list)
  (map car assoc-list))

(define (values assoc-list)
  (map cdr assoc-list))

(define (get-assoc key assoc-list)
  (define (assoc-helper assoc-list)
    (cond
      ((empty-assoc? assoc-list) #f)
      ((equal? (caar assoc-list) key) (cdar assoc-list))
      (else (assoc-helper (cdr assoc-list)))))
  (assoc-helper assoc-list))