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

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (+ (car tup1) (car tup2))
                  (tup+
                   (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (> (sub1 x) (sub1 y))))))

(define <
  (lambda (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) #t)
      (else (< (sub1 x) (sub1 y))))))

(define =
  (lambda (x y)
    (cond
      ((> x y) #f)
      ((< x y) #f)
      (else #t))))

(add1 1)
(sub1 4)
(zero? 0)
(zero? 2)
(+ 46 12)
(- 46 12)
(addtup '(1 2 3))
(multiply 3 4)
(tup+ '(1 2 3) '(4 5 6))
(tup+ '(1 2) '(4 5 6))
(tup+ '(1 2 3) '(5 6))
(> 10 20)
(> 15 7)
(> 3 3)
(< 10 20)
(< 15 7)
(< 3 3)
(= 10 20)
(= 15 7)
(= 3 3)

