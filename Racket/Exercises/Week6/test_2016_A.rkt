#lang racket
;Задача 1. Да се напише функция (meetTwice? f g a b),
;която проверява дали в целочисления интервал [a; b] съществуват две различни цели числа x и y такива, че f(x) = g(x) и f(y) = g(y).

;Пример:(meetTwice? (lambda(x)x) (lambda(x) (- x)) -3 1) → #f

;Пример:(meetTwice? (lambda(x)x) sqrt 0 5) → #t

(define (1+ x)(+ x 1))

(define (accumulate op nv a b next)
  (if (> a b)
      nv
      (op a (accumulate op nv (next a) b next))))

(define (count a b p?)
  (accumulate (lambda(x acc)(if (p? x) (+ 1 acc) acc))0 a b 1+))

(define (meetTwice? f g a b)
  (> (count a b (lambda(x)(= (f x) (g x)))) 1))

;Задача 2. Да се напише функция (maxDuplicate ll), която по списък от списъци от цели числа ll намира най-­голямото от тези числа,
;които се повтарят в рамките на списъка, в който се срещат. Ако в нито един списък няма повтарящи се числа, функцията да връща #f.

;Пример: (maxDuplicate ‘((1 2 3 2) (-­4 -­4) (5))) → 2

;Пример: (maxDuplicate ‘((1 2 3) (-­4 -­5 -6) ())) → #f

(define (remove el l)
  (filter (lambda(x)(not (equal? el x))) l))


(define (duplicate el l)
  (let ((m (member el l)))
    (and (not (null? m)) (member el (cdr m)))))

(define (duplicates l)
  (filter (lambda(x)(duplicate x l)) l))

(define (my-max l)
  (and (not (null? l)) (apply max l)))

(define (maxDuplicate ll)
  (my-max (apply append (map duplicates ll))))

;Задача 3. Да се напише функция (checkMatrix? m k) която проверява дали на всеки ред в дадена матрица m от цели числа има поне по едно число, кратно на k.
;Пример:(checkMatrix? ‘((1 2 6) (3 8 9) (10 12 11)) 3) → #t
;Пример:(checkMatrix? ‘((1 2 4) (3 8 9) (10 12 11)) 3) → #f

(define (all? p? l)
  (let ((len (length l)))
    (= len (length (filter p? l)))))

(define (exists? p? l)
  (not (null? (filter p? l))))

(define (checkMatrix? m k)
  (define (k-divisible? x)
    (= 0 (remainder x k)))
  (define (valid-row? row)
    (exists? k-divisible? row))
  (all? valid-row? m))

;Задача 4. Да се напише функция (longestDescending­ l), която намира низходящо сортиран подсписък на списъка от числа l с максимална дължина.
;Ако съществуват няколко такива подсписъка, функцията да върне първия отляво надясно.
;Упътване: Реализирайте помощна функция, която намира най-дългия низходящо сортиран префикс на даден списък.
;Пример: (longestDescending­ ‘(5 3 8 6 4 2 6 7 1)) → (8 6 4 2)
;Пример: (longestDescending­ ‘(1 2 3 4 5 6)) → (1)

(define (descending l)
  (define (descending* l res)
    (if (> (car l) (car res))
        (reverse res)
        (descending* (cdr l) (cons (car l) res))))
  (descending* (cdr l) (list (car l))))

(define (longest-descending l)
  (define (choose lst1 lst2)
    (if (> (length lst2) (length lst1)) lst2 lst1))
  (foldr (lambda(idx res)(choose (descending (drop l idx)) res)) '() (range (length l))))


