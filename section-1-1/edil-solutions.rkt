#lang sicp

;; For debugging purposes
(define (print . args)
  (cond ((not (null? args))
         (display (car args))
         (apply print (cdr args)))))

;; Exercise 1.1
;; 10
;; 10

;;(+ 5 3 4)
;; 12

;; (- 9 1)
;; 8

;; (/ 6 2)
;; 3

;; (+ (* 2 4) (- 4 6))
;; 6

;; (define a 3)

;; (define b (+ a 1))

;; (+ a b (* a b))
;; 19

;; (= a b)
;; #f

;; (if (and (> b a) (< b (* a b)))
;;     b
;;     a)
;; 4

;; (cond ((= a 4) 6)
;;       ((= b 4) (+ 6 7 a))
;;       (else 25))
;; 16 

;; (+ 2 (if (> b a) b a))
;; 6 

;; (* (cond ((> a b) a)
;;          ((< a b) b)
;;          (else -1))
;;    (+ a 1))
;; 16 


;; Exercise 1.2
;(/ (+ 5 4 (- 2
;             (- 3
;                (+ 6 (/ 4 5)))))
;   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
(define (square x) (* x x ))

(define (f x1 x2 x3)
  (if (> x1 x2)
      (if (> x2 x3)
          (+ (square x1) (square x2))
          (+ (square x1) (square x3)))
      (if (> x3 x1)
          (+ (square x2) (square x3))
          (+ (square x1) (square x2)))))
         
;; Exercise 1.4
;; (define (a-plus-abs-b a b)
;;   ((if (> b 0) + -) a b))
;; If b is positive, return a + b.
;; If b is negative, return a - b.

;; Exercise 1.5
;; (define (p) (p))
;; (define (test x y)
;;   (if (= x 0)
;;       0
;;       y))
;; (test 0 (p))
;;
;; If the interpreter is applicative order, (test 0 (p)) should evaluate its arguments
;; before evaluating the (test) function. Therefore, as (p) is an infinite loop, it should
;; become stuck on an infinite loop.
;; On the other hand, if the interpreter is normal order, (test) will be evaluated first
;; and return 0 without ever evaluating (p).

;; Exercise 1.6
;; (define (new-if predicate then-clause else-clause)
;;   (cond (predicate then-clause)
;;         (else else-clause)))
;; The interpreter is applicative order, thus it will evaluate both the predicate, the
;; then-clause and the else-clause before passing control to cond. The else-clause is recursive
;; and will always be evaluated, no matter if the guess is already good enough. The program
;; becomes stuck in an infinite loop. To work correctly, the program should not recurse if
;; the guess is already good enough which only occurs with the special form if: if only evaluates
;; one of its clauses, but not both.

;; Exercise 1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
;;  (print guess)
;;  (print "\n")
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

;; This is ok
(print "(sqrt 0.01) should be 0.1\n")
(sqrt 0.01)
;; This is clearly wrong, it supposed to be 0.03162277
(print "(sqrt 0.001) should be 0.0316277\n")
(sqrt 0.001)
;; Also wrong
(print "(sqrt 2000000000000000000000000) should be 1414213562373.095048\n")
; Next yields and infinite loop
;;(sqrt 2000000000000000000000000)

;; New implementation
(define (good-enough-2? p-guess guess x)
  (< (abs (- p-guess guess)) 0.001))
(define (sqrt-iter2 p-guess guess x)
  (if (good-enough-2? p-guess guess x)
      guess
      (sqrt-iter2 guess
                  (improve guess x)
                  x)))
(define (sqrt2 x) (sqrt-iter2 0.0 1.0 x))

;; Exercise 1.8
(define (cube-root-iter p-guess guess x)
  (if (good-enough-2? p-guess guess x)
      guess
      (cube-root-iter guess
                      (improve-cube guess x)
                      x)))
(define (improve-cube guess x)
  (print guess)
  (print "\n")
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))
(define (cuberoot x)
  (cube-root-iter 0 1.0 x))