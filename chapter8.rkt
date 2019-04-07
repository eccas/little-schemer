#lang racket

(require "atom.rkt"
         (only-in "chapter4.rkt" eqan?)
         (only-in "chapter5.rkt" equal?)
         (only-in "chapter6.rkt"
                  1st-sub-exp
                  2nd-sub-exp
                  operator))

(provide rember-f
         eq?-c
         insertL-f
         insertR-f
         insert-g
         insertL
         insertR
         subst
         value)

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

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((test? (car lat) old) (cons new lat))
      (else (cons (car lat)
                  ((insertL-f test?) new old (cdr lat))))))))


(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) old) (cons old
                                     (cons new (cdr lat))))
        (else (cons (car lat)
                    ((insertR-f test?) new old (cdr lat))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((equal? (car lat) old) (seq new old (cdr lat)))
        (else (cons (car lat)
                    ((insert-g seq) new old (cdr lat))))))))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR (insert-g seqR))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst
  (insert-g seqS))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else expt))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a
                                 (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test? (cdr lat)))))))

(module+ test
  (rember-f-orig eq? 'b '(a b c))
  (rember-f-orig eqan? '2 '(1 2 3 a b c))
  (rember-f-orig equal? '(a b) '(1 (2 3) (a b) c))
  ((eq?-c '1) '1)
  ((eq?-c '1) '2)
  ((rember-f eq?) 'b '(a b c))
  ((rember-f eqan?) '2 '(1 2 3 a b c))
  ((rember-f equal?) '(a b) '(1 (2 3) (a b) c))
  ((insertL-f equal?) '44 '(a b) '(1 (2 3) (a b) c))
  ((insertR-f equal?) '44 '(a b) '(1 (2 3) (a b) c))
  ((insert-g seqL) '44 '(a b) '(1 (2 3) (a b) c))
  ((insert-g seqR) '44 '(a b) '(1 (2 3) (a b) c))
  (multiremberT (eq?-c '2) '(1 2 3 4 1 2 3 4))
  )