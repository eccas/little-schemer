#lang racket

(require "atom.rkt")

(provide rember
         firsts
         insertR
         insertL
         subst
         subst2
         multirember
         multiinsertR
         multiinsertL
         multisubst)

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                   (rember a(cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old
                                 (cons new (cdr lat))))
      (else (cons (car lat)
                  (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat)
                  (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                   (multirember a
                                (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old
                                 (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new
                                 (cons old
                                       (multiinsertL new old
                                                     (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new
                                 (multisubst new old
                                             (cdr lat))))
      (else (cons (car lat)
                  (multisubst new old
                              (cdr lat)))))))

(module+ test
  (rember 'and '(bacon lettuce and tomato))
  (firsts '( (a b) (c d) (e f) ))
  (insertR 'topping 'fudge '(ice cream with fudge for dessert))
  (insertL 'topping 'fudge '(ice cream with fudge for dessert))
  (subst 'topping 'fudge '(ice cream with fudge for dessert))
  (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
  (multirember 'and '(bacon and lettuce and tomato))
  (multiinsertR 'topping 'fudge '(ice cream fudge with fudge for dessert))
  (multiinsertL 'topping 'fudge '(ice cream fudge with fudge for dessert))
  (multisubst 'topping 'fudge '(ice cream fudge with fudge for dessert)))
