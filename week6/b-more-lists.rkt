#lang racket


(define (takeWhile p? l)
  (define (helper res l)
    (cond ((null? l) res)
          ((p? (car l)) (helper (append res (list (car l)) ) (cdr l)))
          (else res)
          )
    )
  (helper '() l)
)

(define (takeWhile2 p? xs)
  (cond ((or (null? xs) (not (p? (car xs)))) '())
        (else (cons (car xs) (takeWhile2 p? (cdr xs))))))

(define (reverse xs)
  (define (loop xs res)
    (if (null? xs)
        res
        (loop (cdr xs) (cons (car xs) res))))
  (loop xs '()))

(define (append l1 l2)
  (define (helper l res)
    (if (null? l)
        res
        (helper (cdr l) (cons (car l) res))))
  (helper (reverse l1) l2))

(define (append** xs ys)
  (cond ((and (null? xs) (null? ys)) '())
        ((not (null? xs)) (cons (car xs) (append (cdr xs) ys)))
        (else (cons (car ys) (append xs (cdr ys))))))


(define (append* xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (take n xs) 
  (define (helper i l)
    (cond ((null? l) l)
          ((= i n) '())
          (else (cons (car l) (helper (+ i 1) (cdr l))))))
  (helper 0 xs)
  )

;selection sort

(define (remove-first x xs)
  (define (helper curr before after)
    (if (= x curr)
        (append before after)
        (helper (car after) (append before (list curr)) (cdr after))
        )
  )
  (helper (car xs) '() (cdr xs))
  )

(define (find-min xs)
  (define (helper curr l)
    (cond ((null? l) curr)
          ((<= (car l) curr) (helper (car l) (cdr l)))
          (else (helper curr (cdr l)))
          )
    )
  (helper (car xs) (cdr xs))
  )

(define (selection-sort lst)
  (define (helper lst)
    (if (null? lst)
        '()
        (let ((min-value (find-min lst)))
        (cons min-value (helper (remove-first min-value lst) ))))
    )
  (helper lst)
  )

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))


(require rackunit)
(require rackunit/text-ui)

; Искаме функция, която ни казва дали поне един елемент в даден списък
; изпълнява някакво условие

(define (any? p? xs)
  (cond ((null? xs) #f)
        ((p? (car xs)) #t)
        (else (any? p? (cdr xs))))

  )

; Искаме функция, която ни казва дали всички елементи в списък удовлетворяват
; дадено условие
; Дефинирайте я на база на any?


(define (all? p? xs)
  (cond ((null? xs) #t)
        ((not (p? (car xs))) #f)
        (else (all? p? (cdr xs))))
)


(define any?-tests
  (test-suite ""
    (check-true (any? odd? '(2 4 4 2 8 9 2 0)))
    (check-true (any? (lambda (x) (> (length x) 3)) '((1 2 3) (3 4 4 2) (2 1))))
    (check-false (any? (lambda (x) (> x 2)) (map (lambda (x) (remainder x 3)) (range 1 100))))
  )
)

(define all?-tests
  (test-suite ""
    (check-true (all? (lambda (x) (> x 100)) (range 101 103)))
  )
)

;(run-tests any?-tests 'verbose)
;(run-tests all?-tests 'verbose)

; zip
; може да видим какво се очаква да прави в тестовете

(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (cons (car xs) (cons (car ys) '())) (zip (cdr xs) (cdr ys))))
)

(define zip-tests
  (test-suite "Zip"
    (check-equal? (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
    (check-equal? (zip '(28 9 12) '(1 3)) '((28 1) (9 3)))
  )
)

;(run-tests zip-tests 'verbose)


(define (filter p? ys)
  (cond ((null? ys) '())
        ((p? (car ys)) (cons (car ys) (filter p? (cdr ys))))
        (else (filter p? (cdr ys)))
        )
)

(define filter-tests
  (test-suite "filter"
    (check-equal? (filter even? '(1 2 3))  '(2))
    (check-equal? (filter (lambda (x) (> x 200)) '(1 2 3))  '())
    (check-equal? (filter (lambda (x) (or (= x 1) (= x 3))) '(1 2 3))  '(1 3))
  )
)

;(run-tests filter-tests 'verbose)

; remove-duplicates 
; премахва всички повтарящи се елементи от списъка

(define (remove-duplicates xs)
  (define (helper l)
    (cond ((null? (cdr l)) (list (car l)))
          ((equal? (car l) (cadr l)) (helper (cdr l)))
          (else (append (list (car l)) (helper (cdr l))))))
  (helper (selection-sort xs))
  )

(define dup-tests
  (test-suite "remove-duplicates"
    (check-equal? (remove-duplicates '(1 1 2 2 1 3 3 2 3))  '(1 2 3))
    (check-equal? (remove-duplicates '(1 2 3))  '(1 2 3))
  )
)

;(run-tests dup-tests 'verbose)


; chunk 
; разбива списъка xs на подсписъци с дължина n

(define (chunk n xs)
  (define (helper i l res)
    (cond ((null? l) (cons res '()))
          ((= i n) (cons res (helper 0 l '())))
          (else (helper (+ i 1) (cdr l) (append res (list (car l)))))))
  (helper 0 xs '())
)

(define chunk-tests
  (test-suite "chunk"
    (check-equal? (chunk 2 '(1 2 3 4 5 6 7 8 9))  '((1 2) (3 4) (5 6) (7 8) (9)))
    (check-equal? (chunk 3 '(1 2 3 4 5 6 7 8 9))  '((1 2 3) (4 5 6) (7 8 9)))
  )
)

(run-tests chunk-tests 'verbose)



