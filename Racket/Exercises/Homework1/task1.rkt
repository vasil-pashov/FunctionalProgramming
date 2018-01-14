#lang racket
(define (argmin fun lst)
  (define (argmin-helper arg val lst)
    (if (null? lst)
        arg
        (let* ((head (car lst))
              (new-val (fun head))
              (tail (cdr lst)))
          (if (< new-val val)
              (argmin-helper head new-val tail)
              (argmin-helper arg val tail)))))
  (if (null? lst) void (argmin-helper (car lst) (fun (car lst)) (cdr lst))))

(define (argmax fun lst)
  (argmin (lambda(x)(-(fun x))) lst))