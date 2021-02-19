#lang racket

;from prev ex
(define (any? p? lst)
  (cond
    ((null? lst) #f)
    ((p? (car lst)) #t)
    (else (any? p? (cdr lst))))
  )


;6

(define (uniques lst)
  (define (helper orig res)
    (if (null? orig)
        res
        (helper (cdr orig)
                (if (any? (lambda (el) (equal? el (car orig))) res)
                    res
                    (cons (car orig) res)))))
  (helper lst '()))

(define (uniques-1 lst)
  (if (null? lst) '()
      (let [(rest (uniques-1 (cdr lst)))]
        (if (member (car lst) rest)
            rest
            (cons (car lst) rest)))))

(define (uniques-2 lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (uniques-2 (filter (lambda (x) (not (equal? x (car lst)) ) )
                               (cdr lst))))) )

(define (uniques-i lst)
  (define (helper lst res)
    (cond [(null? lst) res]
          [(member (car lst) res) (helper (cdr lst) res)]
          [else (helper (cdr lst) (cons (car lst) res))]
        ))
  (helper lst '()))



;7
(define (insert val lst)
  (cond ((null? lst) (cons val lst))
        (( < val (car lst)) (cons val lst))
        (else (cons (car lst) (insert val (cdr lst)))))
      )

;--------------------------
(define (foldr op nv lst)
  (if  (null? lst)
       nv
       (op (car lst)
           (foldr op nv (cdr lst)))))

(define (maximum lst)
  (if (null? lst) #f
        (foldr max (car lst) (cdr lst)))
)

(define (uniques* lst)
  (foldr (lambda (el res)
           (if (member el res)
               res
               (cons el res)))
         '()
         lst))

(define (length lst)
  (foldr (lambda (el res) (+ 1 res)) 0  lst))

(define (map* f lst)
  (foldr (lambda (el res) (cons (f el) res)) '() lst))

(define (filter* p? lst)
  (foldr (lambda (el res) (if(p? el) (cons el res) res)) '() lst))

(define (insertion-sort lst)
  (foldr insert '() lst))

(define (quicksort lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (append (quicksort (filter (lambda (x) (< x (car lst))) (cdr lst)))
              (list (car lst))
              (quicksort (filter (lambda (x) (>= x (car lst))) (cdr lst)))
              )))




