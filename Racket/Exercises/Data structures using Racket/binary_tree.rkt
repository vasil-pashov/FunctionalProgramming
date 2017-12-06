#lang racket
;Not ordered binary tree
;Representation (value left right)

;correctness checking
(define (tree? t)
  (or (null? t)
       (and (= (length t) 3)
            (tree? (cadr t))
            (tree? (cadr t))
            )))

(define empty-tree? null?)

;constructors
(define empty-tree '())

(define (make-tree root left right)
  (list root left right))

;selectors
(define root car)
(define left cadr)
(define right caddr)

;other operations
(define (tree-depth t)
  (if (empty-tree t) 0
      (+ 1 (max ((tree-depth (left t))) ((tree-depth (right t)))))))

(define (subtree r t)
  (cond
    ((empty-tree? t) #f)
    ((= (root t) r) t)
    (else (or (subtree r (left t))
          (subtree r (right t))))))

(define (cons#f el l) (and l (cons el l)))

(define (path r t)
  (cond
    ((empty-tree? t) #f)
    ((eq? (root t) r) (list r))
    (else (cons#f (root t) (or
                   (path r (left t))
                   (path r (right t)))))))





