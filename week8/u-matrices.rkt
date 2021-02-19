#lang racket/base
(require rackunit)
(require rackunit/text-ui)

; Искаме да вземем главния диагонал на матрица

(define (diagonal matrix)
  (let (
        (rows (length matrix))
        )
    (define (helper l i)
      (cond ((= i (- rows 1)) (list (list-ref l i)))
            (else (append (list (list-ref l i)) (helper (list-ref matrix (+ i 1)) (+ i 1))))))
    (if (null? matrix)
        '()
        (helper (list-ref matrix 0) 0)
        )
    )
)

(define tests-diagonal
  (test-suite "Diagonal tests"

    (test-case "" (check-equal? (diagonal '()) '()))
    (test-case "" (check-equal? (diagonal '((1 2 3) (4 5 6) (7 8 9))) '(1 5 9)))
  )
)

;(run-tests tests-diagonal 'verbose)

; Да се напише функция (sub-matrix i1 j1 i2 j2 m), която намира подматрицата на m
; със зададени горен ляв и долен десен ъгъл (i1,j1) и (i2,j2), съответно.

(define (get-rows i j m)
  (if (> i j)
      '()
      (cons (list-ref m i) (get-rows (+ i 1) j m))))

(define (get-cols i j m)
  (if (> i j)
      '()
      (cons (map (lambda (t) (list-ref t i)) m) (get-cols (+ i 1) j m))))


(define (submatrix i1 j1 i2 j2 m)
  (if (null? m)
      '()
      (get-rows i1 i2 (apply map list (get-cols j1 j2 m)))))


(define tests-submatrix
  (test-suite "Submatrix tests"

    (test-case "" (check-equal? (submatrix 0 1 2 2 '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))) '((2 3) (6 7) (10 11))))
  )
)

;(run-tests tests-submatrix 'verbose)

;Напишете функция (foldr-matrix op-rows nv-rows op-elems nv-elems m),
;която свива матрицата m, като прилага двуместната функция op-elems върху елементите във всеки ред,
;с начална стойност nv-elems. Резултатът от свиването на всеки от редовете се насъбира от двуместната функция op-rows,
;с начална стойност nv-rows


(define (foldr-matrix op-rows nv-rows op-elems nv-elems m)
  (let (
        (rows (map (lambda (t) (foldr op-elems nv-elems t)) m))

        )
    (foldr op-rows nv-rows rows)
    )
  )

(define (any? p? lst)
  (and (not (null? lst))
       (or (p? (car lst))
           (any? p? (cdr lst)))))

(define (all? p? lst)
  (not (any? (lambda (x) (not (p? x))) lst)))

(define (find-submatrix ps m)
  (define (is-Ok el)
    (any? (lambda (p) (p el)) ps))
  (define (is-Ok-mat el)
    (all? (lambda (t) (all? is-Ok t)) m))
  (void))




