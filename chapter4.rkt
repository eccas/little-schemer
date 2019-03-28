#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define +
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (add1 (+ x (sub1 y)))))))

(define -
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (sub1 (- x (sub1 y)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (+ (car tup) (addtup (cdr tup)))))))

(define multiply
  (lambda (x y)
    (cond
      ((zero? y) 0)
      (else
       (+ x (multiply x (sub1 y)))))))

(add1 1)
(sub1 4)
(zero? 0)
(zero? 2)
(+ 46 12)
(- 46 12)
(addtup '(1 2 3))
(multiply 3 4)