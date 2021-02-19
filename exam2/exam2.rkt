#lang racket/base

(provide (all-defined-out)) ; Запазете този ред, за да може всички функции,
                            ; които сте написали, да се експортират и да може да се тестват.


; Това е примерна функция. Можете да напишете решението си на нейното място.

;1
(define (left-tree tree) (cadr tree))
(define (right-tree tree) (caddr tree))


(define (height tree)
  (cond ((null? tree) 0)
        (else (+ 1 (max (height (left-tree tree)) (height (right-tree tree))))))

  )

(define (weight-balanced? tree)
  (cond ((null? tree) #t)
        ((> (abs (- (height (left-tree tree)) (height (right-tree tree)))) 1) #f)
        (else (and (weight-balanced? (left-tree tree)) (weight-balanced? (right-tree tree)) ))))



;2

(define (contains-el el l)
  (not (null? (filter (lambda (x) (equal? x el)) l))))
      
(define (times-found el l)
  (foldl (lambda (t res) (if (equal? (car t) el)
                             (+ res 1)
                             res))
         0
         l)
  )

(define (attempts subj res)
  (let* (
        (all (filter (lambda (t) (equal? (list-ref t 1) subj)) res))
        (ids (map (lambda (t) (list (list-ref t 0))) all))
        (unique-ids (foldl (lambda (x res) (if (contains-el x res)
                                   res
                                   (cons x res)))
                           '()
                           ids))
        )
    (foldl (lambda (t res) (cons (cons (car t) (times-found (car t) ids)) res)) '() unique-ids)
    )

  )




