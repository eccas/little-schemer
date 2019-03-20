#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(atom? (quote ()))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? '(a b))
(lat? '())
(lat? '(a b (c d)))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(member? 'a '(b c a))
(member? 'a '(b c ab))
(member? 'a '())
