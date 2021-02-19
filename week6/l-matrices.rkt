#lang racket/base

(define test-matrix '((1 2 3 4) (5 6 7 8)))

(define (number-rows M)
  (length M))

(define (number-cols M)
  (if (null? M)
      0
      (length (car M))))

(define (first-row M)
  (car M))

(define (remove-first-row M)
  (cdr M))

(define (get-row n M)
  (list-ref M n))

(define (first-col M)
  (map car M))

(define (remove-first-col M)
  (map cdr M))

(define (get-column n M)
  (map (lambda (t) (list-ref t n)) M))

(define (get-element M n m)
  (list-ref (list-ref M n) m))

(define (remove-nth n l)
  (cond ((null? l) '())
        ((= n 0) (cdr l))
        (else (cons (car l) (remove-nth (- n 1) (cdr l))))))

(define (remove-row M n)
  (remove-nth n M))

(define (remove-col M n)
  (map (lambda (t) (remove-nth n t)) M))

(define (transpose M)
  (apply map list M))