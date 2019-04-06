#lang racket

(require "atom.rkt"
         "chapter5.rkt"
         (only-in "chapter3.rkt" firsts))

(provide set?
         makeset
         subset?
         eqset?
         intersect?
         intersect
         union
         difference
         intersectall
         a-pair?
         first
         second
         third
         build
         fun?
         revpair
         revrel
         fullfun?
         seconds
         one-to-one?)

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

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))


(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1  set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))
      
(define difference
  (lambda (set1  set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1)
                  (difference (cdr set1) set2))))))

(define intersectall
  (lambda (lset)
    (cond
      ((null? (cdr lset)) (car lset))
      (else (intersect (car lset)
                       (intersectall (cdr lset)))))))

(define a-pair?
  (lambda (l)
    (cond
      ((atom? l) #f)
      ((null? l) #f)
      ((null? (cdr l)) #f)
      ((null? (cdr (cdr l))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2
                '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (p)
    (build (second p)
           (first p))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons
             (revpair (car rel))
             (revrel (cdr rel)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (second (car l))
                  (seconds (cdr l)))))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

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
  (intersect? '(a b c) '(1 2 b))
  (intersect? '(a b c) '(1 2 d))
  (intersect '(a b c) '(1 2 b a))
  (intersect '(a b c) '(1 2 d))
  (union '(a b c) '(1 2 b a))
  (union '(a b c) '(1 2 d))
  (difference '(a b c) '(1 2 b a))
  (difference '(a b c) '(1 2 d))
  (intersectall '((a b c)(1 2 b d a)(h b a)))
  (intersectall '((a b c)(1 2 d a)))
  (intersectall '((a b c)))
  (a-pair? '(a b))
  (a-pair? '(a b c))
  (a-pair? '(a (b c)))
  (a-pair? '(a))
  (a-pair? 'a)
  (first '(a b))
  (second '(a b))
  (build 'a 'b)
  (third '(a b c))
  (fun? '((a b)(c d)(e f)(g h)))
  (fun? '((a b)(c d)(e f)(a h)))
  (revrel '((a b)(c d)(e f)(g h)))
  (fullfun? '((a b)(c d)(e f)(a h)))
  (fullfun? '((a b)(c d)(e f)(a b)))
  (one-to-one? '((a b)(c d)(e f)(a h)))
  (one-to-one? '((a b)(c d)(e f)(a b)))
  )
