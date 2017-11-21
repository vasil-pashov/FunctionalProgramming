# Exercises from Week 3 of the FMI course in Functional programming

Original task page https://github.com/Andreshk/FunctionalProgramming2017/blob/master/ex3.md

**Ex. 1.** Write a function **```(reverse-int n)```**, which reverses the order of digits of a given number:
```
(reverse-int 1234) -> 4321
(reverse-int 10000) -> 1
```
**Ex..** Write a function **```(palindrome? n)```**, which checks if given number is a palindrome.
```
(palindrome? 12321) -> #t
(palindrome? 4040) -> #f
```
**Ex. .** Write a function **```(divisors-sum n)```**, which finds the sum of the divisors of a given number:
```
(divisors-sum 12) -> 28 ;1+2+3+4+6+12=28
```
**Ex. 4.** Write a function **```(perfect? n)```**, which checks if a number is perfec e.i. the sum of its divisors (without the number) equals the number and a function  **```(n_perfects n)```** returning the firs n perfect numbers in a list:
```
(perfect? 6) -> #t ;1+2+3=6
(perfect? 28) -> #t ;1+2+4+7+14=28
(perfect? 33550336) -> #t ;1+2+...+16775168=33550336

(n_perfects 4) -> '(8128 496 28 6)
```
**Ex. 5.** Write a function **```(prime? n)```**, which checks if given number is prime, and a function **```(n-primes n)```** which gives the first n prime numbers i a list:
```
(prime? 1) -> #f ;(!)
(prime? 2) -> #t
(prime? 101) -> #t

(n-primes 10) -> '(23 19 17 13 11 7 5 3 2 1)
```
**Ex. 6.** Write a function **```(increasing? n)```**, which checks if the digits of a given number are in increasing order (left to right):
```
(increasing? 12489) -> #t
(increasing? 4456) -> #f
```
**ЗEx. 7.** Write a functin **```(toBinary n)```**, which converts number from decimal to binary:
```
(toBinary 8) -> 1000
(toBinary 15) -> 1111
(toBinary 42) -> 101010
```
**Ex. 8.** Write a function which **```(toDecimal n)```**, converts from binary to decimal (included in file 07_binary_dec.rkt):
```
(toDecimal 101010) -> 42
(toDecimal (toBinary 1234)) -> 1234
```
**Ex. 9.** Write predicates **```(for-all? predicate? a b)```**, **```(exists? predicate? a b)```** which returns correspondingly, true if the predicate is true for all numbers in range [a,b], true if exists number in range [a,b] for which the predicate is true. Write function **```(count predicate? a b)```** which returns the count of numbers for which predicate is true in range [a,b].
```
(for-all? (lambda(x)(x < 10)) 1 9) -> #t
(exists? odd? 1 10) -> #t
(count even? 1 10) -> 5
```
