(define (take n lst)
  (cond ((null? lst) '())
        ((= n 0) '())
        (else (cons (car lst) (take (- n 1) (cdr lst))))
      ))

(define (drop n lst)
  (cond ((null? lst) '())
        ((= n 0) lst)
        (else (drop (- n 1) (cdr lst))))
      )


;2
(define (all? p? lst)
  (if (null? lst)
      #t
      (if (not (p? (car lst)))
          #f
          (all? p? (cdr lst)))))


(define (any? p? lst)
  (if (null? lst)
      #f
      (if (p? (car lst))
          #t
          (any? p? (cdr lst)))))

;3

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))))
;4

(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (car lst1) (car lst2)) (zipWith (cdr lst1) (cdr lst2)))))

;5

(define (sorted? lst)
  (define (helper l prev)
    (if (null? l)
        #t
        (if (< (car l) prev)
            #f
            (helper (cdr l) (car l)))))
  (if (not (null? (cdr lst)))
      (helper (cdr lst) (car lst) )
      #t)
    )





