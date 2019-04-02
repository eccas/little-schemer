#lang racket

(provide atom?)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(module+ test
  (atom? (quote ()))
  (atom? '(a b))
  (atom? 'a)
  (atom? 1))