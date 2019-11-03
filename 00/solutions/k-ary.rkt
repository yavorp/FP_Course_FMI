#lang racket
(define (pow n k)
    (if (= k 0) 1 
        (* n (pow n (- k 1)))))
(provide from-k-ary
         to-k-ary)
(define (from-k-ary n k)
  (define (iter i res tmp)
    (if (= tmp 0) res
        (iter (+ 1 i) (+ res (* (pow k i) (remainder tmp 10))) (quotient tmp 10))))
  (iter 0 0 n))
(define (to-k-ary n k)
    (define (iter i res tmp)
        (if (= tmp 0) res
            (iter (+ 1 i) (+ res (* (pow 10 i) (remainder tmp k))) (quotient tmp k))))
    (iter 0 0 n))