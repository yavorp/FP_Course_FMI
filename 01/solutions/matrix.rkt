#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix)
; the provide "exports" these functions

; 00.

(define (cols xss)
  (apply map list xss))

(define (all? p? l)
  (if (null? l) #t
      (and (p? (car l)) (all? p? (cdr l)))))


(define (any? p? l)
  (if (null? l) #f
      (or (p? (car l)) (any? p? (cdr l)))))


(define (concat ll)
  (if (null? ll) '()
      (append (car ll) (concat (cdr ll)))))

(define (rows matrix) matrix)

(define (matrix-ref matrix row col)
  (if (= row 0) (list-ref (car matrix) col)
      (matrix-ref (cdr matrix) (- row 1) col)))

(define (set xs i x)
  (if (and (null? xs) (= i 0))
      (list x)
      (if (= i 0)
          (cons x (cdr xs))
          (cons (car xs) (set (cdr xs) (- i 1) x)))))

(define (place xss i j x)
  (if (= i 0)
      (append (list (set (car xss) j x)) (cdr xss))
      (append (list (car xss)) (place (cdr xss) (- i 1) j x))))

(place '((1 2 3)
         (4 5 6)
         (7 8 9)) 1 1 42)

(define (drop xs n)
  (if (= n 0) xs
      (drop (cdr xs) (- n 1))))

(define (diag xss)
  (define (helper tmp row)
    (if (= row (length xss))
        '()
        (cons (list-ref (car tmp) row) (helper (cdr tmp) (+ row 1)))))
  (helper xss 0))

(define (diags xss)
  (define len (length xss))
  (define (helper tmp row)
    (if (= row len)
        '()
        (cons (list-ref (car tmp) (- len row 1))
              (helper (cdr tmp) (+ row 1)))))
  (cons (diag xss) (list (helper xss 0))))

(define (filter-matrix p? xss)
  (if (null? xss)
      '()
      (cons (filter p? (car xss))
            (filter-matrix p? (cdr xss)))))

(define (map-matrix f xss)
  (if (null? xss)
      '()
      (cons (map f (car xss)) (map-matrix f (cdr xss)))))

(define (zip-with f xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (f (car xs) (car ys))
            (zip-with f (cdr xs) (cdr ys)))))

(define (zip-matrix xss yss)
  (define (helper tmpx tmpy rows)
    (if (= rows (length xss))
        '()
        (cons (zip-with cons (car tmpx) (car tmpy))
              (helper (cdr tmpx) (cdr tmpy) (+ rows 1)))))
  (helper xss yss 0))
