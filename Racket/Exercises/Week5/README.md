# Exercises from Week 5 of the FMI course in Functional programming

**Ex. 0.** Write functions **```(length n)```**, **```(reverse n)```**, **```(map n)```**, **```(filter n)```**  which act like the built in ones:

**Ex. 1.** Write function **```(nth n lst)```**, whick returns nth element in list or false if there is no nth element.
```
(nth 0 '(1 2 3)) -> 1
(nth 5 '(1 2 3)) -> #f
```
**Ex. 2.** Write function **```(range from to)```**, which returns list of elements in the interval from to inclusive:
```
(range 5 10) -> '(5 6 7 8 9 10)
```
**Ex. 3.** Write function **```(digit-list n)```**, which returns the digits of a number in list 
```
(digit-list 1806) -> '(1 8 0 6)
```
**Ex. 4.** Write functions **```(take n lst)```**, which takes the first n elements of list and **```(drop n lst)```**, which removes the first n elements of list
```
(take 3 (range 1 5)) -> '(1 2 3)
(take 10 (range 1 5)) -> '(1 2 3 4 5)
(drop 3 (range 1 5)) -> '(4 5)
(drop 10 (range 1 5)) -> '()
```
**Ex. 5.** Write functions **```(all? p? lst)```**, which checks if predicate p? is true for all elements of list lst
      and **```(any? p? lst)```**, which checks if predicate p? is true for any of the elements in list lst:
```
(all? even? '(1 2 3 4 5)) -> #f
(any? even? '(1 2 3 4 5)) -> #t
(any? (lambda (x) (> x 10)) '(4 2 6 3 1)) -> #f
```
**ЗEx. 6.** Write functions **```(zip-with f l1 l2)```**, which returns the in list the result of applying function f to each two corresponding elements of list l1 and l2,
            and **```(zip l1 l2)```** which returns list of pairs with each corresponding elements of lists l1 and l2:
```
(zipWith + '(1 2 3 4) '(7 10 12)) -> '(8 12 15)
(zip '(1 2 3 4) '(#t #f #f)) -> '((1 . #t) (2 . #f) (3 . #f))
```
**Ex. 7.** Write a predicate which **```(sorted? lst)```** which checks if the list lst is sored in increasing order:

```
(sorted? '(1 2 3 4)) -> #t
(sorted? '(1 3 2 4)) -> #f
```
**Ex. 8.** Write function **```(uniques lst)```** which returns list with elements of list lst occuring only once:
```
(uniques '(1 2 2 "iei" 1 3 "iei" 'oops)) -> '(1 2 "iei" 3 'oops)
```

**Ex. 9.** Write function **```(extract-ints lst)```** which returns list with only the integers of list lst:
```
(extract-ints '(1 2 #t "Hello" (3 "no") #f 4 (5))) -> (1 2 4)
```

**Ex. 10.** Write functions **```(insert val lst)```** which inserts val in the correct place in the sorted list lst and
        **```(insertion-sort lst lst)```** which uses **```(insert val lst)```** to implements insertion sort
```
(insert 5 '(1 4 10)) -> '(1 4 5 10)
(insert 12 '(1 4 10)) -> '(1 4 10 12))
(insertion-sort '(4 3 6 2 1 8 10)) -> '(1 2 3 4 6 8 10)
```
**Ex. 11.** Write function **```(merge-sort lst)```** which implements merge sort algorithm

**Ex. 12.** Write function **```(quick-sort lst)```** which implements quick sort algorithm

**Ex. 13.** Write function **```(my-arity . xs)```** which returns the length of its arguments
```
(my-arity 1 2 #f "iei" (3 5)) -> 5
```

**Ex. 14.** Write function **```(compose . fns)```** which creates composition of its parameters:

```
(define (sq x) (* x x))
(define (1+ x) (+ x 1))
(define f (compose sq 1+ (lambda (x) (* x 2)) 1+))
(f 5) -> 169
```
**Ex. 15.** Write function **```(group-by f lst)```** which groups the elements of list lst by the return value of f
```
(group-by even? '(1 2 3 4 5)) -> ((#f (1 3 5))
                                  (#t (2 4)))
(group-by length '((1 2 3) (4) (5 6 7))) -> '((1 ((4)))
                                              (3 ((1 2 3) (5 6 7))))
```

**Ex. 16.** Write function **```(zip-with f . lsts)```** which has no fixed arity 
```
(zip-with list '(1 2 3) '(a b) '(7 8 9 10)) -> '((1 a 7) (2 b 8))
(zip-with* + '(1 2 3)) -> '(1 2 3)
(zip-with* void) -> '()
```



















