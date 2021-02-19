#lang racket/base

(provide (all-defined-out)) ; Запазете този ред, за да може всички функции,
                            ; които сте написали, да се експортират и да може да се тестват.


; Това е примерна функция. Можете да напишете решението си на нейното място.
(define (left-tree tree) (cadr tree))
(define (right-tree tree) (caddr tree))
(define (make-tree root left right) (list root left right))
(define (tree? tree)
  (or (null? tree)
      (and (list? tree)
           (= (length tree) 3)
           (tree? (left-tree tree))
           (tree? (right-tree tree)))))

(define (invert tree)
  (cond ((not (tree? tree)) #f)
        ((null? tree) '())
        (else (make-tree (car tree) (invert (right-tree tree)) (invert (left-tree tree))))))

; прави пълно дърво с дадена височина и всичко стойности във върховете са дадената стойност
(define (full-tree level value)
  ( cond ((= level 0) '())
         (else (make-tree value (full-tree (- level 1) value) (full-tree (- level 1) value))))
)

; Да се напише функция extremum, която по даден списък от списъци от числа намира число, което е минимално или максимално във всеки от списъците, ако има такова, или 0 иначе
; (extremum '((1 2 3 2) (3 5) (3 3) (1 1 3 3))) → 3

;           '((1 2 3 2) (3 5) (3 3) (1 1 3 3)))
;candidates:  '(1 3)    '(3)  '(3)   '(3) 


(define (min-el l)
  (define (helper l res)
    (cond ((null? l) res)
          ((< (car l) res) (helper (cdr l) (car l)))
          (else (helper (cdr l) res))))
  (cond ((null? l) 0)
        ((null? (cdr l)) (car l))
        (else (helper (cdr l) (car l))))
)

(define (max-el l)
  (define (helper l res)
    (cond ((null? l) res)
          ((> (car l) res) (helper (cdr l) (car l)))
          (else (helper (cdr l) res))))
  (cond ((null? l) 0)
        ((null? (cdr l)) (car l))
        (else (helper (cdr l) (car l))))
)

(define (is-extremum el l)
  (or (= (max-el l) el) (= (min-el l) el))
  )


(define (extremum l)
  (let (
        (first-max (if (null? l)
                          0
                          (max-el (car l))))
        (first-min (if (null? l)
                          0
                          (min-el (car l))))
        )
    (define (check-max l)
      (cond ((null? l) #t)
            ((or (= first-max (max-el (car l))) (= first-max (min-el (car l)))) (check-max (cdr l)))
            (else #f)
            ))
    (define (check-min l)
      (cond ((null? l) #t)
            ((or (= first-min (max-el (car l))) (= first-min (min-el (car l)))) (check-min (cdr l)))
            (else #f)
            ))

    (cond ((null? l) 0)
          ((check-max l) first-max)
          ((check-min l) first-min)
          (else 0))

    )
  ;'((1 2) (1 2) (1 2))

  )


(define (tree?* tree)
  (or (null? tree)
        (and  (list? tree)
              (= (length tree) 3)
              (tree? (left-tree tree))
              (tree? (right-tree tree))
              ))
        )

(define (tree->list tree)
  (if (null? tree)
      '()
      (append 
              (tree->list (left-tree tree))
              (list (car tree))
              (tree->list (right-tree tree))
              )))

(define (sorted? l)
  (cond ((null? (cdr l)) #t)
        ((> (car l) (cadr l)) #f)
        (else (sorted? (cdr l)))))

(define (ordered? tree)
  (let (
        (l (tree->list tree))
        )
    (sorted? l)
    ))

(define (height tree)
  (cond ((null? tree) 0)
        (else (+ (max (height (left-tree tree))
                      (height (right-tree tree))) 1))))

(define (balanced? tree)
  (cond ((null? tree) #t)
        ((> (abs (- (height (left-tree tree)) (height (right-tree tree)))) 1) #f)
        (else (and (balanced? (left-tree tree)) (balanced? (right-tree tree))))))

;          1
;       2
;     3

