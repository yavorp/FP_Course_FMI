#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))
(define one (lambda (f v) (f v)))
(define two (lambda (f v) (f (f v))))

(define (succ n)
  (lambda (f v)
    (f (n f v))))
(define (1+ x) (+ x 1))
(define (from-numeral n)
  (n 1+ 0))

(define (to-numeral n)
  (if (= n 0) zero
      (succ (to-numeral (- n 1)))))

(define (plus n m)
  (n succ (m succ zero)))

(define (mult n m)
  (lambda (f nv)
    (n (lambda (x) ((m succ zero) f x)) nv)))

(define (pred n) (if (= (from-numeral n) 0) zero (to-numeral (- (from-numeral n) 1))))
