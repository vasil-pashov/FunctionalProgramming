#lang racket
(define empty? null?)
(define root car)
(define left cadr)
(define right caddr)
(define (leaf? tree)
  (and (empty? (left tree))
       (empty? (right tree))))

(define t '(1 (5 (8 () (2 (0 () ()) (11 () ()))) ()) (7 (12 () ()) (13 () ()))))
(define t1 '(1 (5 (8 () (2 (0 () ()) (11 () ()))) ()) (7 (12 () ()) (13 (123 () ()) (321 () ())))))
(define t2 '(1 (2 (9 () ()) (15 () ())) (7 (9 () ()) ())))


(define (preorder tree)
  (if (empty? tree)
      '()
      (append (list (root tree)) (preorder (left tree)) (preorder (right tree)))))

(define (inorder tree)
  (if (empty? tree)
      '()
      (append (inorder (left tree)) (list (root tree)) (inorder (right tree)))))

(define (postorder tree)
    (if (empty? tree)
      '()
      (append (postorder (right tree)) (list (root tree)) (postorder (left tree)))))

(define (level tree n)
  (define (helper tree lvl)
    (cond
      ((empty? tree) '())
      ((= lvl n) (list (root tree)))
      (else (append (helper (left tree) (+ 1 lvl)) (helper (right tree) (+ 1 lvl))))))
  (helper tree 0))

(define (count-leaves tree)
  (cond
   ((empty? tree) 0)
   ((leaf? tree) 1)
   (else (+ (count-leaves (left tree)) (count-leaves (right tree))))))

(define (map-tree f tree)
  (if (empty? tree)
      '()
      (list (f (root tree)) (map-tree f (left tree)) (map-tree f (right tree)))))

(define (height tree)
  (if (empty? tree)
      0
      (+ 1 (max (height (left tree)) (height (right tree))))))

(define (sum-tree t)
  (apply + (inorder t)))

(define (max-tree t)
  (apply max (inorder t)))

(define (max-tree* tree)
  (cond
    ((leaf? t) (root tree))
    ((empty? (left t)) (max (root tree) (max-tree* (right t))))
    ((empty? (right t)) (max (root tree) (max-tree* (left t))))
    (else (max (left tree) (right tree)))))

(define (invert tree)
  (if (empty? tree)
      '()
      (list (root tree) (invert (right tree)) (invert (left tree)))))

(define (binary-heap? tree)
  (cond
    ((empty? tree) #t)
    ((leaf? tree) #t)
    ((and (empty? (left tree))) (> (root (right tree)) (root tree)) (binary-heap? (right tree)))
    ((and (empty? (right tree))) (> (root (left tree)) (root tree)) (binary-heap? (left tree)))
    (else (and
      (> (root (left tree)) (root tree))
      (> (root (right tree)) (root tree))
      (binary-heap? (left tree))
      (binary-heap? (right tree))))))

(define (balanced? tree)
  (if (empty? tree)
      #t
      (and (balanced? (left tree))
           (balanced? (right tree))
           (< (abs (- (height (left tree)) (height (right tree)))) 2))))

(define (ordered? tree)
  (define (helper tree interval)
  (if (empty? tree)
      #t
      (and (in-interval? interval (root tree))
           (helper (left tree) (cons (car interval) (root tree)))
           (helper (right tree) (cons (root tree) (cdr interval))))))
  (helper tree (cons -inf.0 +inf.0)))

(define (in-interval? int x)
  (and (>= x (car int)) (<= x (cdr int))))

(define (ordered*? tree)
  (sorted? (inorder tree)))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))))

(define (all? p? lst)
  (null? (filter (lambda(x)(not (p? x))) lst)))

(define (sorted? lst)
  (all? (lambda(x)(< (car x) (cdr x))) (zip lst (cdr lst))))


