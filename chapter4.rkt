#lang racket

(require "atom.rkt")

(provide +
         -
         addtup
         multiply
         tup+
         >
         <
         =
         ^
         length
         pick
         rempick
         no-nums
         all-nums
         eqan?
         occur
         one?
         rempick2)

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

(define ^
  (lambda (x y)
    (cond
      ((zero? y) 1)
      (else (multiply x (^ x (sub1 y)))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons
             (car lat)
             (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
      (= n 1)))

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons
             (car lat)
             (rempick2 (sub1 n) (cdr lat)))))))

(module+ test
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
  (^ 1 1)
  (^ 2 3)
  (^ 5 3)
  (length '(a b c d e f))
  (length '())
  (length '(a b))
  (pick 4 '(a b c d e f))
  (pick 1 '(a b c d e f))
  (rempick 4 '(a b c d e f))
  (rempick 1 '(a b c d e f))
  (no-nums '(a 1 b 2 c 3 d e))
  (all-nums '(a 1 b 2 c 3 d e))
  (eqan? 1 2)
  (eqan? 1 1)
  (eqan? 1 'a)
  (eqan? 'a 'b)
  (eqan? 'a 'a)
  (occur 1 '(a f 2 1 j 3 1))
  (one? 1)
  (one? 12)
  (rempick2 4 '(a b c d e f))
  (rempick2 1 '(a b c d e f)))
