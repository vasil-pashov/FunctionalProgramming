#lang racket
(require "associative_list.rkt")

(define g '((1 2 3)
(2 3 6)
(3 4 6)
(4 1 5)
(5 3)
(6 5)
(7 8)
(8)))

(define gsymm '((1 2 3 4)
(2 1 6)
(3 1 6)
(4 1 5)
(5 6 4)
(6 5 2 3)))

(define (nodes g)
  (keys g))

(define (children node g)
  (get-assoc node g))

(define (edge? u v g)
  (member v (children u g)))

(define (map-children u f g)
  (map f (children u g)))

(define (search-child p u g)
  (search p (children u g)))

(define (symmetryc? g)
  (all? (lambda(node)
          (all?
           (lambda(child)(edge? child node g))
           (children node g)))
        (nodes g)))

;Remmebers the current path in order to avoid cycle
(define (dfs-path u v g)
  (define (dfs current-path)
    (if (equal? (car current-path) v)
        (reverse current-path)
        (search-child
         (lambda (child)
           (and
            (not (member child current-path))
            (dfs (cons child current-path))))
         (car current-path) g)))
  (dfs (list u)))

(define (bfs-path u v g)
  (define (next-level paths)
    (apply append (map (lambda(path)(extend-path path g)) paths)))
  (define (target-path path)
    (and (equal? (last path) v) path))
  (define (bfs-path-helper paths)
    (and (not (null? paths))
         (or (search target-path  paths)
             (bfs-path-helper (next-level paths)))))
  (bfs-path-helper (list (list u))))

(define (simple-paths g)
  (define (next-level level)
    (apply append (map (lambda(path)(extend-path path g)) level)))
  (define (simple-paths-helper level)
    (if (null? level)
        '()
        (append level (simple-paths-helper (next-level level)))))
  (simple-paths-helper (map list (nodes g))))



(define (extend-path path g)
  (map (lambda(child)(append path (list child)))
       (filter (lambda(child)(not (member child path))) (children (last path) g))))

(define (last lst)
  (car (reverse lst)))

(define (search p? lst)
  (and (not (null? lst))
       (or (p? (car lst))
           (search p? (cdr lst)))))

(define (all? p? lst)
  (= (length lst) (length (filter p? lst))))

(define (any? p? lst)
  (not (null? (filter p? lst))))

