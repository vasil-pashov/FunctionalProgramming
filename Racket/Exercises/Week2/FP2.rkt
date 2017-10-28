#lang racket

;Фибоначи с помощна функция
(define (fib x)
  (define (fib2 f1 f2 x)
            (if (= x 0)
                f1
                (fib2 f2 (+ f2 f1) (- x 1))
            )
  )
  (fib2 0 1 x)
)

;Помощна функция за търсене на делител на X в интервала от N до 2
(define (has-divisor? n x)
    (define (div? x d)
        (and (> d 0) (= 0 (remainder x d)))
    )
    (cond ((< n 2)   #f)
          ((div? x n) #t)
          (else (has-divisor? (- n 1) x))
    )
)  

(define (prime? x)
  (not (has-divisor? (ceiling (sqrt x)) x))
)

;Проверка къде попада точка в координатната равнина
(define (locate x y)
  (if (= x 0)
      (if (= y 0) "O" "Oy")
      (if (= y 0) "Ox"
           (if (> x 0)
               (if (> y 0) "I" "IV")
               (if (> y 0) "II" "III")
           )
       )
  )
)


; Бързо степенуване
(define (pow x y)
  (define (sqr x) (* x x))
  (if (< y 0)
      (/ 1 (pow x (- y)))
      (if (= y 0) 1
          (if (= 1 (remainder y 2))
              (* x (pow x (- y 1)))
              (sqr (pow x (/ y 2)))
          )
       )
  )
)


;Решава квадратно уравнение
(define (solve a b c)
  ;частен случай, когато е линейно
  (define (solve-lin)
    (cond ((and (= b 0) (= c 0)) "Inf!")
          ((= b 0) "No!")
          (else (/ (- c) b))
    )
  )
  ; Дискиминанта
  (define D (- (* b b) (* 4 (* a c))))
  ; и самото решение
  (if (= a 0)(solve-lin)
      (if (> D 0) 
          (let ((D-sq (sqrt D)))
               (cons (/ (+ (- b) D-sq) (* a 2))
                     (/ (- (- b) D-sq) (* a 2))
               )
          )
          (if (< D 0) "NRR!" (/ (- b) (* 2 a)))
      )
  )
)