#lang sicp

;; For debugging purposes
(define (print . args)
  (cond ((not (null? args))
         (display (car args))
         (apply print (cdr args)))))


;; Exercise 1.9

;;(define (+ a b)
;;  (if (= a 0)
;;      b
;;      (inc (+ (dec a) b))))
;; 
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; This is a recursive process.

;;(define (+ a b)
;;  (if (= a 0)
;;      b
;;      (+ (dec a) (inc b))))
;;
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; This is an iterative process.


;; Exercise 1.10
(print "Exercise 1.10\n")

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(print "\n(A 1 10)\n")
(A 1 10)

(print "\n(A 2 4)\n")
(A 2 4)

(print "\n(A 3 3)\n")
(A 3 3)

;; (define (k n) (* 5 n n))
;; (k n) = 5*n^2

(define (f n) (A 0 n))
;; (f n) = 2*n
(print "\n(f 4) = 2*4 = 8\n")
(f 4)

(define (g n) (A 1 n))
;; (g n) = (A 0 (A 1 (n-1)) =
;; 2*(A 1 (n-1)) =
;; 2*(A 0 (A 1 (n-2))) = 2*2*(A 1 (n-2)) =
;; ... = 2^n
(print "\n(g 4) = 2^4 = 16\n")
(g 4)

(define (h n) (A 2 n))
;; (h n) = (A 1 (A 2 (n-1))) =
;; (A 1 (A 1 (A 2 (n-2)))) =
;; (A 1 (A 1 (... (A 2 1) ...))) =
;; (A 1 (A 1 (... (A 1 2) ...))) =
;; (A 1 (A 1 (... (A 0 (A 1 1) ...))) =

;; (A 2 0) = 0
;; (A 2 1) = 2
;; (A 2 2) = (


;; Exercise 1.11
; Recursive version
(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1))
         (* 2 (f1 (- n 2)))
         (* 3 (f1 (- n 3))))))
; Iterative version
(define (f2 n)
  (f2-iter 0 1 2 n))
(define (f2-iter n3 n2 n1 n)
  (cond ((< n 3) n)
        ((= n 3) (+ n1 (* 2 n2) (* 3 n3)))
        (else (f2-iter n2 n1 (+ n1
                                (* 2 n2)
                                (* 3 n3))
                       (- n 1)))))
  