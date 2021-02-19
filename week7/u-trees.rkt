#lang racket/base

(provide (all-defined-out)) ; Запазете този ред, за да може всички функции,
                            ; които сте написали, да се експортират и да може да се тестват.

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (list root '() '()))
(define (root tree) (car tree))
(define (left-tree tree) (cadr tree))
(define (right-tree tree) (caddr tree))

(define (tree? tree)
  (or (null? tree)
      (and (list? tree)
           (= (length tree) 3)
           (tree? (left-tree tree))
           (tree? (right-tree tree))))
  )

; искаме да проверим дали нещо се среща в дърво
; във всяка от задачите започваме с проверка за празно дърво
; елемент се среща в дърво, тогава когато:
; - или е корена на дървото
; - или се среща в лявото, или дясното поддърво (среща се = member-tree?)

(define (member-tree? x tree)
  (and (not (null? tree)) (or (= x (car tree)) (member-tree? x (left-tree tree)) (member-tree? x (right-tree tree))))
  )

; искаме да намерим сумата на всички елементи в дървото

(define (sum-tree* tree)
  (define (helper tree)
    (if (null? tree)
        0
        (if (and (null? (left-tree tree)) (null? (right-tree tree)))
            (car tree)
            (+ (car tree) (sum-tree (left-tree tree)) (sum-tree (right-tree tree))))))
  (if (tree? tree)
      (helper tree)
      #f)
      )

(define (sum-tree tree)
  (define (helper tree)
    (cond ((null? tree) 0)
          (else (+ (car tree) (sum-tree (left-tree tree)) (sum-tree (right-tree tree)))))
    )
  (if (tree? tree)
      (helper tree)
      #f)
      )

; искаме да намерим всички елементи на дадено ниво в дървото
; тук имахме две дъна:
; - отново гледаме за празно дърво
; - проверяваме дали сме поискали ниво 0 - тогава е окей да върнем корена на дървото ни (след като знаем, че не е празно)
; - иначе комбинираме (с append) резултатите за лявото и дясното поддърво, но с по-малко ниво

(define (tree-level n tree)
  (cond ((null? tree) '())
        ((= n 0) (cons (car tree) '()))
        (else (append (tree-level (- n 1) (left-tree tree)) (tree-level (- n 1) (right-tree tree))))
      ))

; искаме да приложим функцията f върху всички елементи на дървото (като истинската map, ама за дървета)
(define (map-tree f tree)
  (cond ((null? tree) '())
        (else (list (f (car tree)) (map-tree f (left-tree tree)) (map-tree f (right-tree tree)))))
  )


; искаме да върнем списък от елементите на дървото - ляво, корен, дясно
(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-tree tree)) (list (car tree)) (tree->list (right-tree tree)))))

; искаме да проверим дали х се среща в двоичното наредено дърво tree
; тук правим итеративен процес (опашкова рекурсия)

(define (bst-member x tree)
  (cond ((null? tree) #f)
        ((= (car tree) x) #t)
        ((> x (car tree)) (bst-member x (right-tree tree)))
        ((< x (car tree)) (bst-member x (left-tree tree)))))

  ; искаме да вкараме елемент в двоично наредено дърво (binary search tree - BST)
(define (bst-add x tree)
  (cond ((null? tree) (make-leaf x))
        ((> x (car tree)) (make-tree (car tree) (left-tree tree) (bst-add x (right-tree tree))))
        (else (make-tree (car tree) (bst-add x (left-tree tree)) (right-tree tree))))
  
  )

; искаме да сортираме даден списък, използвайки tree->list и bst-insert
(define (sort l)
  (define (helper l tree)
    (if (null? l)
        tree
        (helper (cdr l) (bst-add (car l) tree))))
  (tree->list (helper l '())))

;Да се напише функция (valid-bst? t), която проверява дали дървото t е валидно двоично наредено дърво.

(define (valid-bst? t)
  (if (tree? t)
      (equal? (tree->list t) (sort (tree->list t)))
      #f)
  
  )

;Да се напише функция (prune t), която премахва всички листа в дървото t.

(define (prune t)
  (if (and (null? (left-tree t)) (null? (right-tree t)))
      '()
      (make-tree (car t)
                 (cond ((null? (left-tree t)) '())
                               (else (prune (left-tree t))))
                 (cond ((null? (right-tree t)) '())
                       (else (prune (right-tree t)))))))

;Да се напише функция (bloom t), която заменя всяко листо със стойност x със следното дърво:
(define (bloom t)
  (if (and (null? (left-tree t)) (null? (right-tree t)))
      (make-tree (car t) (make-leaf (car t)) (make-leaf (car t)))
      (make-tree (car t)
                 (cond ((null? (left-tree t)) '())
                       (else (bloom (left-tree t))))
                 (cond ((null? (right-tree t)) '())
                       (else (bloom (right-tree t)))))))

; Да се напише функция (avg t), която заменя всяка стойност във възлите на дадено дърво
; със средно-аритметичното на максималната и минималната стойност в поддървото с корен съотвеетния възел.



