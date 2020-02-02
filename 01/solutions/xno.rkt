#lang racket

(provide winner
         play)

(require "matrix.rkt")
; You can use your matrix functions below, thanks to the "require" invocation above.

(define (id x) x)
(define (foldl op nv term xs)
  (if (null? xs) nv
      (foldl op (op nv (term (car xs))) term (cdr xs))))
; winner implementation that only detects draws right now.
; Put your own implementation here!
(define (winner b)
  (if (is-winner b "X") "X"
          (if (is-winner b "O") "O"
              (if (andmap (lambda (xs) (andmap id xs)) b)
                  "D"  #f))))
(define (my-or a b) (or a b))
(define (every? p? xs)
  (if (null? xs) #t
      (and (p? (car xs)) (every? p? (cdr xs)))))
(define (is-winner b symbol)
  (or (foldl my-or #f (lambda(xs) (every? (lambda (x) (equal? x symbol)) xs)) b)
      (foldl my-or #f (lambda(xs) (every? (lambda (x) (equal? x symbol)) xs)) (cols b))
      (foldl my-or #f (lambda(xs) (every? (lambda (x) (equal? x symbol)) xs)) (diags b))))
; "Dumb" "AI", plays the "next" free spot, going left-to-right, top-to-bottom.
; Put your own implementation here!

;noobs solution for finding empty spaces :(

(define (inverse sign)
  (if (equal? sign "X") "O"
      "X"))

(define (my-filter xs i)
  (if (null? xs) '()
      (if (equal? (car xs) #f)
          (cons i (my-filter (cdr xs) (+ 1 i)))
          (my-filter (cdr xs) (+ 1 i)))))


(define (get-empty-spaces-matrix b i)
  (if (null? b) '()
      (append (map (lambda (x) (cons i x)) (my-filter (car b) 0))  
              (get-empty-spaces-matrix (cdr b) (+ 1 i)))))

(define (max op l)
  (if (null? (cdr l))
      (car l)
      (if (op (car l) (max op (cdr l)))
          (car l)
          (max op (cdr l)))))
(define (inverse-result x)
  (if (= x 1) -1
      (if (= x -1) 1
          0)))

(define (best-outcome b curr-sign)
  (define empties (get-empty-spaces-matrix b 0))
  (cond ((equal? (winner b) curr-sign) 1)
        ((equal? (winner b) (inverse curr-sign)) -1)
        ((equal? (winner b) "D") 0)
        (else (inverse-result (max < (map (lambda (x) (best-outcome (place b (car x) (cdr x) curr-sign)
                                             (inverse curr-sign))) empties)))))) 

(best-outcome '(
                   ("O" "X" "O")
                   ("O" "X" #f)
                   (#f "O" "X"))
              "X")
(define (get-sign value)
  (if value "X" "O"))

(define (play curr-board curr-sign)
  (define empties (get-empty-spaces-matrix curr-board 0))
  (define sign (get-sign curr-sign))
  (car (max (lambda (x y) (< (cdr x) (cdr y)))
            (map (lambda (x) (cons x 
                                      (best-outcome
                                       (place curr-board (car x) (cdr x) sign)
                                       (inverse sign)))) empties))))

(play '(
                   ("X" "O" "X")
                   ("X" "O" #f)
                   (#f #f #f))
              #f)
