#lang racket
;Задача 1. Да се напише функция (min-sum-digit a b k), която намира най-малкото от целите числа от a да b, чиято сума на цифрите се дели на k.

(define (digit-sum n)
  (if (= n 0)
      n
      (+ (remainder n 10) (digit-sum (quotient n 10)))))

(define (min-sum-digit a b k)
  (cond
    ((> a b) #f)
    ((= (remainder (digit-sum a) k) 0) a)
    (else (min-sum-digit (+ 1 a) b k))))

;Задача 2. Да се напише функция (average f g), която по две числови функции f : R → R и g : R → R  намира средно-аритметичната функция
;(f⊕g)(x)=f(x)+g(x)2(f⊕g)(x)=f(x)+g(x)2. С помощта на average да се напише функция от по-висок ред (calcprod f n), която намира произведението
;∏ni=1(f⊕gi)(i)∏i=1n(f⊕gi)(i), където gi(x) = ix. Използването на accumulate е позволено, но не е задължително.

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (average f g)
  (lambda(x) (/ (+ (f x) (g x)) 2)))

(define (fun-mult f g)
  (lambda(x)(* (f x) (g x))))

(define (one x) 1)
(define (1+ x)(+ x 1))

(define (calcprod f n)
  (accumulate fun-mult one 1 n (lambda(i) (average f (lambda(x)(expt i x)))) 1+))

;Задача 3. Да се дефинира функция (occurrences l1 l2). l1 и l2 са списъци от числа.
;Функцията да конструира списък с броя на срещанията на всеки от елементите на l1 в l2.
;Пример: (occurrences ‘(1 2 3) ‘( 1 2 4 1 )) -> (2 1 0)

(define (count n lst)
  (foldr (lambda(el res) (if (equal? n el) (+ 1 res) res)) 0 lst))

(define (occurrences l1 l2)
  (map (lambda(x)(count x l2)) l1))