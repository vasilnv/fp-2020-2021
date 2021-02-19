#lang racket/base
; Граф за нас ще означава списък от двойки (ребра)
; например '((1 . 2) (2 . 3) (2 . 5) (2 . 4) (4 . 3) (5 . 4))

(define g '((1 . 2) (2 . 3) (2 . 5) (2 . 4) (4 . 3) (5 . 4)))

; търсим входна степен на даден връх - колко ребра влизат в него
(define (in-degree g v)
  (foldr (lambda (p res) (cond ((= (cdr p) v) (+ res 1))
                               (else res)))
         0
         g)
  )

; търсим изходна степен на даден връх - колко ребра излизат от него
(define (out-degree g v)
  (foldr (lambda (p res) (cond ((= (car p) v) (+ res 1))
                               (else res)))
         0
         g)

  )


; искаме списък с всички върхове на g
(define (flatten g)
  (cond ((null? g) '())
        ((pair? g) (append (flatten (car g)) (flatten (cdr g))))
        (else (list g))))

(define (find-min l)
  (define (helper l res)
    (cond ((null? l) res)
          ((< (car l) res) (helper (cdr l) (car l)))
          (else (helper (cdr l) res))))
  (helper (cdr l) (car l))
    )
  
(define (rm-el x l)
  (cond ((null? l) '())
        ((= x (car l)) (cdr l))
        (else (cons (car l) (rm-el x (cdr l)))))
)

(define (sort g)
  (let (
        (min-el (find-min g))
        )
    (cond ((null? g) '())
          ((null? (cdr g)) (cons (car g) '()))
          (else (cons min-el (sort (rm-el min-el g))))))

    )

(define (nodes g)
  (let (
        (l (sort (flatten g)))
        )
    (define (helper l)
      (cond ((null? (cdr l)) (cons (car l) '()))
            ((= (car l) (cadr l)) (helper (cdr l)))
            (else (cons (car l) (helper (cdr l)))))
      )
   
    (helper l)
   )
  )

; преобразуваме g към представяне със списък на съседство
  (define (neighbours v g)
    (let ((edges-starting-with-v (filter (lambda (edge) (= (car edge) v)) g)))
    (map cdr edges-starting-with-v))
  )

(define (adjacency-list g)
  (let (
        (all-nodes (nodes g))
        )
    (map (lambda (t) (list t (neighbours t g))) all-nodes)
    
    ))
