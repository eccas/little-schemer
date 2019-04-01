#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and
             (numbered? (car aexp))
             (numbered? (car (cdr (cdr aexp)))))))))

(define multiply
  (lambda (x y)
    (cond
      ((zero? y) 0)
      (else
       (+ x (multiply x (sub1 y)))))))

(define ^
  (lambda (x y)
    (cond
      ((zero? y) 1)
      (else (multiply x (^ x (sub1 y)))))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) 'x) (multiply (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '^) (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value-prefix
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (+ (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'x) (multiply (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '^) (^ (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp)))))))

(numbered? '(1 + 3))
(numbered? '(1 + (3 x 4)))
(numbered? '(1 + cat))
(numbered? '1)

(value '(1 + 3))
(value '(1 + (3 x 4)))
(value '(1 + (3 ^ 4)))
(value '1)

(value-prefix '(+ 1 3))
(value-prefix '(+ 1 (x 4 3)))
