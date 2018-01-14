#lang racket
(define root car)
(define left cadr)
(define right caddr)
(define make-node list)
(define empty-tree? null?)
(define (leaf? t)
  (and (empty-tree? (left t))
       (empty-tree? (right t))))
(define new-tree '())
(define (new-leaf val)
  (list val new-tree new-tree))
(define children->list cdr)
(define (non-empty-children t)
  (filter (lambda(child)(not (empty-tree? child))) (children->list t)))

(define (atomic-operand? operand)
  (not (list? operand)))

(define (multiarg-expr->tree expr)
   (define (helper operands op)
      (cond
        ((null? operands) new-tree)
        ((= (length operands) 1) (operand->node (car operands)))
        (else (make-node op (operand->node (car operands)) (helper (cdr operands) op)))))
  (let ((op (car expr)))
    (helper (cdr expr) op)))

(define (operand->node operand)
  (if (atomic-operand? operand)
      (new-leaf operand)
      (expr->tree operand)))

(define (expr->tree expr)
  (define (helper res expr)
    (if (null? expr)
        res
        (let ((expr-len (length expr))
              (op (car expr)))
          (cond
            ((= expr-len 2) (make-node op (operand->node (cadr expr)) new-tree))
            ((= expr-len 3) (make-node (car expr)
                   (operand->node (cadr expr))
                   (operand->node (caddr expr))))
            (else (helper res (multiarg-expr->tree expr)))))))
  (helper new-tree expr))

(define (tree-eval t x)
  (let ((root (root t)))
      (if (leaf? t)
          (if (equal? 'x root) x root)
          (eval (cons root (map (lambda(child)(tree-eval child x)) (non-empty-children t)))))))

(define (product-rule f g)
  (make-node '+ (make-node '* (tree-derive f) g) (make-node '* f (tree-derive g))))
(define (quotient-rule f g)
  (make-node '/ (make-node '- (make-node '* (tree-derive f) g) (make-node '* f (tree-derive g))) (make-node 'expt g (new-leaf 2))))

(define func-derivatives
  (list (cons 'expt (lambda(t)
    (make-node '* (make-node 'expt (left t) (right t))
               (tree-derive (make-node '* (right t) (make-node 'log (left t) new-tree))))))
     (cons 'log (lambda(t) (make-node '/ (new-leaf 1) (left t))))
     (cons 'exp (lambda(t)(make-node 'exp (left t) new-tree)))
     (cons 'sqrt (lambda(t)
                (make-node '/ (new-leaf 1)
                           (make-node '* (new-leaf 2)
                                      (make-node 'sqrt (left t) new-tree)))))))
  
(define (derive-atomic el) (new-leaf (if (eq? el 'x) 1 0)))

(define (tree-derive t)
  (let ((root (root t)))
    (cond
      ((leaf? t) (derive-atomic root))
      ((or (eq? root '+) (eq? root '-))
       (make-node root (tree-derive (left t)) (tree-derive (right t))))
      ((eq? root '*) (product-rule (left t) (right t)))
      ((eq? root '/) (quotient-rule (left t) (right t)))
      (else (derive-composite t)))))

(define (derive-composite t)
  (let ((fn (root t)))
    (if (eq? fn 'expt)
        ((cdr (assoc fn func-derivatives)) t)
        (make-node '* ((cdr (assoc fn func-derivatives)) t) (tree-derive (left t))))))

(define t1 (expr->tree '(+ x 5)))
(define t2 (expr->tree '(* 2 (+ x 5))))
(define t3 (expr->tree '(*(+ x 5) (* (+ x 2) (* x 3)))))
(define t4 (expr->tree '(*(+ x 5) (* (+ x 2) (/ x (* 2 x))))))
(define t5 (expr->tree '(expt (expt x 2) 3)))
(define t6 (expr->tree '(* (+ (* (+ 3 2) x) x) (expt (+ (+ x (expt x 2)) (* 3 x)) 3))))
(define t7 (expr->tree '(expt (sqrt (exp (* x x x x x))) (log x))))


 