#lang racket

(require "atom.rkt"
         "chapter5.rkt")

(provide set?
         makeset
         subset?
         eqset?)

(module redefs racket
  (provide member?
           multirember)
  
  (define member?
    (lambda (a lat)
      (cond
        ((null? lat) #f)
        (else (or (equal? a (car lat))
                  (member? a (cdr lat)))))))

  (define multirember
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((equal? (car lat) a)
         (multirember a (cdr lat)))
        (else (cons (car lat)
                    (multirember a
                                 (cdr lat))))))))

(require 'redefs)

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

; Original definition using member?
(module+ orig
  (define makeset
    (lambda (lat)
      (cond
        ((null? lat) '())
        ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
        (else (cons (car lat) (makeset (cdr lat))))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)
                  (makeset
                   (multirember (car lat)
                                (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and
             (member? (car set1) set2)
             (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))
   
(module+ test
  (set? '(a b c))
  (set? '(a b c a))
  (set? '(a b 1 c 2 1))
  (set? '(a b 1 c 2))
  (makeset '(a b c))
  (makeset '(a b c a))
  (makeset '(a b 1 c 2 1))
  (subset? '(a c) '(a b c d))
  (subset? '(2 c) '(a 3 b 2 c d))
  (subset? '(2 c r) '(a 3 b 2 c d))
  (eqset? '(a c b) '(a b c))
  (eqset? '(a c 4) '(4 a c))
  (eqset? '(a c b) '(a b c d))
  )
