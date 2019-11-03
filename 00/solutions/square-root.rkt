#lang racket

(provide my-sqrt)

(define (f x target) (- (* x x) target))

(define (my-sqrt x)
  (define (iter current i)
    (if (= i 100) current
        (iter (- current (/ (f current x) (* 2 current))) (+ i 1))))
  (iter 10. 0))