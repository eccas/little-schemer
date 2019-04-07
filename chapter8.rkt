#lang racket

(require "atom.rkt"
         (only-in "chapter4.rkt" eqan?)
         (only-in "chapter5.rkt" equal?))

(provide rember-f
         eq?-c)

(define rember-f-orig
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l)
                  (rember-f-orig test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))))))))

(module+ test
  (rember-f-orig eq? 'b '(a b c))
  (rember-f-orig eqan? '2 '(1 2 3 a b c))
  (rember-f-orig equal? '(a b) '(1 (2 3) (a b) c))
  ((eq?-c '1) '1)
  ((eq?-c '1) '2)
  ((rember-f eq?) 'b '(a b c))
  ((rember-f eqan?) '2 '(1 2 3 a b c))
  ((rember-f equal?) '(a b) '(1 (2 3) (a b) c))

  )