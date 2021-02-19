#lang racket
;------1---------

;Нека имаме следния списък
(define my-list '(1 2 3 (4 5) (6 (7 8))))

; Искаме с подходящи извиквания на car и cdr да вземем всяко число.
; Първите две са за пример.
(define get-one (car my-list))

(define get-two (cadr my-list))

(define get-three (caddr my-list))

(define get-four (car (car (cdr (cdr (cdr my-list))))))

(define get-five (car (cdr (car (cdr (cdr (cdr my-list)))))))

(require rackunit)
(require rackunit/text-ui)

; Търсим дължината на даден списък.

(define (length xs)
  (define (helper l res)
    (if (null? l)
        res
        (helper (cdr l) (+ res 1))))
  (helper xs 0)
)

(define tests-length
  (test-suite "dummy tests"
    (check-equal? (length '()) 0)
    (check-equal? (length '(1 2)) 2)
    (check-equal? (length '(3 2 1 2 3 9 3 #f)) 8)
  )
)

;(run-tests tests-length 'verbose)

; Търсим сумата на числата от даден списък

(define (sum-elements xs)
  (define (helper l res)
    (if (null? l)
        res
        (helper (cdr l) (+ res (car l)))))
  (helper xs 0)

)

(define tests-sum
  (test-suite "Sum elements tests"
    (check-equal? (sum-elements (range 1 6)) 15)
    (check-equal? (sum-elements '(1 9)) 10)
    (check-equal? (sum-elements '(-2 3 -1)) 0)
  )
)

;(run-tests tests-sum 'verbose)


(define (member? x lst)
  (cond ((null? lst) #f)
        ((= (car lst) x) #t)
        (else (member? x (cdr lst)))
      )
)

(define tests-member
  (test-suite "member tests"
    (check-true (member? 2 (range 1 6)))
    (check-false (member? 22 (range 1 20)))
  )
)

;(run-tests tests-member 'verbose)


; Искаме да вземем i-тия елемент от списъка lst, като броим от 0.

(define (list-ref lst i)
  (define (helper j l)
    (if (= i j)
        (car l)
        (helper (+ j 1) (cdr l))))
  (helper 0 lst)
)

(define tests-list-ref
  (test-suite "List ref tests"
    (check-equal? (list-ref '(5 9 2) 0) 5)
    (check-equal? (list-ref '(1 8 6 2 3) 4) 3)
  )
)

;(run-tests tests-list-ref 'verbose)

(define (last xs)
  (if (null? (cdr xs))
             (car xs)
             (last (cdr xs)))
)

(define tests-last
  (test-suite "List ref tests"
    (check-equal? (last '(5 9 2)) 2)
    (check-equal? (last '(1 8 6 2 3)) 3)
    (check-equal? (last '(1)) 1)
    (check-equal? (last '(2 3)) 3)
  )
)

;(run-tests tests-last 'verbose)
; Търсим функция, която конкатенира два списъка

(define (append xs ys)
  (define (helper res l)
    (if (null? l)
        res
        (helper (cons (car l) res) (cdr l))))
  (helper ys (reverse xs))
)

(define tests-append
  (test-suite "append tests"
    (check-equal? (append '(5 9 2) '(1)) '(5 9 2 1))
    (check-equal? (append '() '(2 3)) '(2 3))
    (check-equal? (append '(2 3) '()) '(2 3))
    (check-equal? (append '(1 8 6 2 3) '(2 3)) '(1 8 6 2 3 2 3))
  )
)

;(run-tests tests-append 'verbose)

; И нейн итеративен вариант
(define (reverse-iter xs)
  (define (helper list res)
    (if (null? list)
        res
        (helper (cdr list) (cons (car list) res))))
  (helper xs '())
)

(define tests-rev
  (test-suite "Reverse tests"
      (check-equal? (reverse-iter '(1 2 3)) (reverse '(1 2 3)))
      (check-equal? (reverse '()) '())
      (check-equal? (reverse '(1)) '(1))
      (check-equal? (reverse '(1 5)) '(5 1))
  )
)

;(run-tests tests-rev 'verbose)


; Търсим функция, която връща списък от първите n елемента на даден такъв.

(define (take n xs)
  (define (helper res i l)
    (if (= i n)
        res
        (helper (cons (car l) res) (+ i 1) (cdr l))))
  (if (> n (length xs))
      xs
      
      (reverse-iter (helper '() 0 xs)))
)

(define tests-take
  (test-suite "Take tests"
     (check-equal? (take 2 '(1 2 3)) '(1 2))
     (check-equal? (take 0 '(2 9 2)) '())
     (check-equal? (take 2134 '(9 7 2 3)) '(9 7 2 3))
  )
)

;(run-tests tests-take 'verbose)


; Търсим функция, която връща списък от всички без първите n елемента на даден такъв.

(define (drop n xs)
  (define (helper l i)
    (if (or (null? l ) (= i n))
        l
        (helper (cdr l) (+ i 1))))
  (helper xs 0)
)

(define tests-drop
  (test-suite "Take tests"
     (check-equal? (drop 2 '(1 2 3 4)) '(3 4))
     (check-equal? (drop 0 '(2 9 2)) '(2 9 2))
     (check-equal? (drop 2134 '(9 7 2 3)) '())
  )
)

;(run-tests tests-drop 'verbose)


; Искаме функция, която приема списък и две числа и връща
; списък, състоящ се от елементите на списъка, които се намират на индекси от първото число до второто.

(define (slice xs start end)
  (define (helper i l res)
    (cond ((null? l) res)
          ((< i start) (helper (+ i 1) (cdr l) res))
          ((<= i end) (helper (+ i 1) (cdr l) (cons (car l) res)))
          (else res)))
  (reverse (helper 0 xs '() ))
  )

(define tests-slice
 (test-suite "Slice tests"
     (check-equal? (slice '(1 9 8 2) 1 2) '(9 8))
     (check-equal? (slice '(1 9 2 8 3) 2 10) '(2 8 3))
     (check-equal? (slice '(9 7 2 3) 0 2) '(9 7 2)) 
  )
)

(run-tests tests-slice 'verbose)
