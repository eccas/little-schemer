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

(define rember
  (lambda (a lat)
    (cond
      (( null? lat ) (quote ( ) ) )
      (( eq? ( car lat) a) ( cdr lat ) )
      (else ( cons ( car lat)
                   ( rember a( cdr lat) ) ) ) ) ) )
(rember 'and '(bacon lettuce and tomato))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (cons (car (car l)) (firsts (cdr l)))))))
(firsts '( (a b) (c d) (e f) ))


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old
                                 (cons new (cdr lat))))
      (else (cons (car lat)
                  (insertR new old (cdr lat)))))))
(insertR 'topping 'fudge '(ice cream with fudge for dessert))       

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat)
                  (insertL new old (cdr lat)))))))
(insertL 'topping 'fudge '(ice cream with fudge for dessert))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst new old (cdr lat)))))))
(subst 'topping 'fudge '(ice cream with fudge for dessert))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst2 new o1 o2 (cdr lat)))))))
(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                   (multirember a
                                (cdr lat)))))))
(multirember 'and '(bacon and lettuce and tomato))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old
                                 (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old (cdr lat)))))))
(multiinsertR 'topping 'fudge '(ice cream fudge with fudge for dessert))

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
(multiinsertL 'topping 'fudge '(ice cream fudge with fudge for dessert))

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
(multisubst 'topping 'fudge '(ice cream fudge with fudge for dessert))