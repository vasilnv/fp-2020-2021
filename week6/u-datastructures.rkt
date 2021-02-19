#lang racket
; data structures


; Matrices
; test matrix - '((1 2 3) (4 5 6) (7 8 9))
(define (head l)
  (car l))

(define (tail l)
  (cdr l))


(define (head-rows m) (head m))
(define (head-cols m) (map head m))
(define (tail-rows m) (tail m))
(define (tail-cols m) (map tail m))
(define (null-m? m) (or (null? m) (null? (head m))))

(define (sub-range i j lst)
  (drop (take lst j) i))

(define (sub-matrix i1 j1 i2 j2 m)
  (let [(the-rows (sub-range i1 ( + i2 1) m))]
    (map (lambda (row) (sub-range j1 j2 row)) the-rows))
)

; Binary trees

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)


  (define test-tree
  (make-tree 10
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

(define (height t)
  (if (empty-tree? t) 0
      (+ 1 (max (height (left-tree t)) (height (right-tree t)))))
  )


;4
(define (tree-sum t)
  (define (helper tree)
    (if (empty-tree? tree)
        0
        (+ (helper (left-tree tree))
           (helper (right-tree tree))
           (root-tree tree))))
  (helper t))

;5
(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(= k 0) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]
      ))

;6
(define (all-levels t)
  (map (lambda (x) (tree-level x t)) (range 0 (height t)))
  )

;7
(define (tree-map f t)
  (if (null? t)
      '()
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))

;8
(define (ordered? tree))