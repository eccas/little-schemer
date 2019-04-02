#lang racket

(require "atom.rkt")

(provide lat?
         member?)

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))


(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(module+ test
  (lat? '(a b))
  (lat? '())
  (lat? '(a b (c d)))
  (member? 'a '(b c a))
  (member? 'a '(b c ab))
  (member? 'a '()))
