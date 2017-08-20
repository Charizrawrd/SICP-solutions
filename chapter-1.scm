
;; Exercise 1.1
;; What is the result printed by each expression?
10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3) ;; a
(define b (+ a 1)) ;; 4
(+ a b (* a b)) ;; 19
(= a b) ;; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ;; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(* (cond ((> a b) a)
        ((< a b) b)
        (else -1))
   (+ a 1)) ;; 16

;;Exercise 1.2
(/ (+ 5 4
    (- 2
       (- 3 (+ 6 (/ 4 3)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; Exercise 1.3
(define (sum-of-squares a b)
  (+ (* a a)
     (* b b)))

(define (drop-lowest a b c)
  (cond ((and (<= a b) (<= a c)) (sum-of-squares b c))
        ((and (<= b c) (<= b a)) (sum-of-squares a c))
        ((and (<= c a) (<= c b)) (sum-of-squares a b))))

;; Exercise 1.4
;; Describe the behavior of the following procedure
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; This procedure will take the abs value of b and add it to a. The "if" statement will return + if the statement is true, and - if it is false. The resulting expression will be either (+ a b) or (- a b) depending on if b is positive or negative respectively.

;; Exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
;; How will this following procedure be evaluated using applicative-order evaluation or normal-order evaluation when passed to the following procedure?
(test 0 (p))
;; When using applicative-order evaluation, the procedure will enter an endless loop. Since (p) calls itself, it will expand indefinitely.
;; When using normal-order evaluation, the procedure will return 0 because it evaluates x to be zero and never looks to (p).

;;Exercise 1.6
;; Alyssa P. Hacker doesn't see why if needs to be provided as a special form. ``Why can't I just define it as an ordinary procedure in terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:
;;(define (new-if predicate then-clause else-clause)
;;  (cond (predicate then-clause)
;;        (else else-clause)))
;;Eva demonstrates the program for Alyssa:
;;(new-if (= 2 3) 0 5)
;;5
;;(new-if (= 1 1) 0 5)
;;0
;;Delighted, Alyssa uses new-if to rewrite the square-root program:
;;(define (sqrt-iter guess x)
;;  (new-if (good-enough? guess x)
;;          guess
;;          (sqrt-iter (improve guess x)
;;                     x)))
;;What happens when Alyssa attempts to use this to compute square roots? Explain.If you were to define if in terms of cond statements,
;; If is a special form that evaluates either the consequent or the alternative. new-if will evaluate both of the consequent expressions and will repeatedly call itself.

;;Exercise 1.7
;;Explain how the good-enough? procedure fails at low and high numbers. Design a procedure that works better at extreme values.
;; At high numbers, the average will equal the improved guess, but will never be within the tolerance of the procedure.
;; At low numbers, the value will be within the tolerance, but far away from the actual square root.
;; The solution to this is to check if the difference between guesses and to stop when it is below the threshold instead of the difference between the square of guess and the actual number is below the tolerance.

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess))
     (abs (* guess 0.001))))

;;Exercise 1.8

(define (cube-root guess previous x)
  (if (good-enough? guess previous)
      guess
      (cube-root (improve guess x) guess x)))

(define (average x y z)
  (/ (+ x y z) 3))

(define (improve guess x)
  (average (/ x (* guess guess)) guess guess))
