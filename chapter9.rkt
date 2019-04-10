#lang racket

(require "atom.rkt"
         (only-in "chapter7.rkt"
                  build
                  first
                  second
                  a-pair?
                  revpair))

(provide looking
         pick
         keep-looking
         shift
         align
         length*
         weight*
         shuffle
         eternity)

(define pick
  (lambda (i lat)
    (list-ref lat (sub1 i))))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define shift
  (lambda (p)
    (build (first (first p))
           (build (second (first p))
                  (second p)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (length* (first pora))
          (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (* (weight* (first pora)) 2)
          (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(define eternity
  (lambda (x)
    (eternity x)))

; length0
((lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
  eternity)

; length<=1
((lambda (f)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (f (cdr l)))))))
  ((lambda (g)
     (lambda (l)
       (cond
         ((null? 0) 0)
         (else (add1 (g (cdr l)))))))
   eternity))

(module+ test
  (looking 'caviar '(6 2 4 caviar 5 7 3))
  (looking 'caviar '(6 2 grits caviar 5 7 3))
  (shift '((a b) c))
  (shift '((a b) (c d)))
  (align '((a b) c))
  (align '((a b) (c d)))
  (align '((a (b1 b2)) (c d)))
  )