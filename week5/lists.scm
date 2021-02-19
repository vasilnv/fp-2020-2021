(define l '(2 #f 1 "iei"))

(define (member? x lst)
  (cond ((null? lst) #f)
        ((= (car lst) x) #t)
        (else member? x (cdr lst))))

(define (my-member x lst)
  (cond ((null? lst) #f)
        ((= (car lst) x) lst)
        (else member? x (cdr lst))))

(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (map f (cdr lst)))))

(define (filter p? lst)
  (cond ((null? lst) '())
        ((p? (car lst)) (cons (car lst) (filter p? (cdr lst))))
        (else (filter p? (cdr lst)))))

(define (my-length lst)
  (define (helper lst res)
    (if (null? lst)
        res
        (helper (cdr lst) (+ res 1))))
  (helper lst 0))

;(map my-length '((1 2 3) (2 3) ("iei" 1 2 2 3 4 5)))

