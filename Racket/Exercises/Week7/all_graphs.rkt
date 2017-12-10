#lang racket

(define G '((a b c d) ; от а има ребра към b,c,d
            (b e f)   ; може да бъде и ориентиран
            (c a d)
            (d b c g)
            (e)       ; връх без наследници
            (f b e)
            (g a)))

(define (nodes g)
  (map car g))

(define (children node g)
  (cdr (assoc node g)))

(define (has-edge u v g)
  (let ((children (children u g)))
    (and children (member v children))))

(define (has-node u g)
  (member u (nodes g)))

(define (add-node u g)
  (if (has-node u g)
      g
      (cons (list u) g)))

(define (add-nodes g u . new-nodes)
  (foldr add-node g (cons u new-nodes)))


(define (add-edge u v g)
  (update-asc-lst u v (add-nodes g u v)))

(define (contains-path? path g)
  (all? (lambda(p)(has-edge (car p) (cdr p) g)) (zip path (cdr path))))

(define (predecessors v g)
  (filter (lambda(node)(has-edge node v g)) (nodes g)))

(define (extend-path path g)
  (let ((next (filter (lambda(x)(not (member x path))) (children (last path) g))))
    (map (lambda(x)(append path (list x))) next)))


(define (extend-level level g)
  (apply append (map (lambda(x)(extend-path x g)) level)))

(define (all-paths g)
  (define (all-paths-helper level g)
    (if (null? level)
        '()
        (append level (all-paths-helper (extend-level level g) g))
        )
    )
  (all-paths-helper (map list (nodes g)) g))

(define (node-edges node g)
  (map (lambda(child)(cons node child)) (children node g)))

(define (edge-list g)
  (apply append (map (lambda(node) (node-edges node g)) (nodes g))))

(define (inverse g)
  (edges->graph (map swap (edge-list g))))

(define (swap pair)
  (cons (cdr pair) (car pair)))

(define (edges->graph edges)
  (foldr (lambda(edge res) (add-edge (car edge) (cdr edge) res)) '() edges))

(define (bfs start g)
  (define (next node visited)
    (filter (lambda(child)(not (member child visited))) (children node g)))
  (define (bfs-help visited q)
    (if (null? q)
        visited
        (let ((to-visit (next (car q) visited)))
          (bfs-help (append visited to-visit) (append (cdr q) to-visit)))))
  (bfs-help (list start) (list start)))

(define (cyclic g)
  (exists? (lambda(path)(exists? (lambda(node)(equal? (car path) node)) (children (last path) g))) (all-paths g)))

(define (acyclic g) (not (cyclic g)))

;slow but working
(define (has-path? u v g)
  (exists? (lambda(path)(and (equal? (car path) u) (equal? (last path) v))) (all-paths g)))

(define (connected g)
  (all? (lambda(u)(all? (lambda(v)(has-path? u v g))) (nodes g)) (nodes g)))

(define (tree? g)
  (and (connected g) (acyclic g)))

(define (exists? p? l)
  (not (null? (filter p? l))))

(define (update-asc-lst key value assc-lst)
  (if (assoc key assc-lst)
      (map
       (lambda(asc) (if (equal? (car asc) key) (update-assc asc value) asc))
       assc-lst)
      (cons (list key value) assc-lst)))

(define (update-assc asc value)
    (if
      (member value asc)
      asc
      (cons (car asc) (cons value (cdr asc)))))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
   '()
   (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))))

(define (all? p? lst)
  (= (length lst) (length (filter p? lst))))