# 99 Scheme problems

This folder contains my solutions to the 99 scheme problems. Link to the scheme site with the problems is: (http://community.schemewiki.org/?ninety-nine-scheme-problems)

Description of the problems:

1. Find the last box of a list.

Example:
```
> (last '(1 2 3 4))
> 4
```
2. Find the last but one box of a list.

Example:
```
> (last-but-one '(1 2 3 4))
> 3
```
3. Find the K'th element of a list. Counting starts from 1.

Example:
```
> (k-th '(a b c d) 3)
> c
```
4. Find the number of elements of a list.

Example:
```
> (len '(1 2 3 4 4))
> 5
```
5. Reverse a list.

Example:
```
> (my-reverse '(a b 3 2))
> '(2 3 b a)
```
6. Find out whether a list is a palindrome.

Example
```
> (palindrome? '(1 2 3))
> #f
> (palindrome? '(1 2 1))
> #t
```
7. Flatten a nested list structure.

Example
```
> (flatten '(1 2 (2 3 (4) (1 (2)))))
> '(1 2 2 3 4 1 2)
> (flatten '(()))
> '()
```
8. Eliminate consecutive duplicates of list elements.

Example
```
> (eliminate_duplicates '(1 1 2 3 1 3 3 3 4 () ()))
> '(1 2 3 1 3 4 ())
```
9. Pack consecutive duplicates of list elements into sublists.

Example
```
> (pack '(() () () () 1 2 2 2 a a (3) (3)))
> '((() () () ()) (1) (2 2 2) (a a) ((3)) ((3)))
```
