#lang racket/base
;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 2
; 2020-12-12
;
; Име: 
; ФН:
; Специалност:
; Курс: 
; Административна група: 
; Начален час на контролното: <тук попълнете часа за вашата група>
;


(require "u-trees.rkt") ; Променете името на файла, който се включва така,
                   ; че тук да се зарежда файлът с вашето решение.
                   ; Например ако файлът с решението ви се казва 12345.rkt,
                   ; променете реда на (require "12345.rkt").

(require rackunit rackunit/gui)

(test/gui

 ; Даденият по-долу пример е само ориентировъчен.
 ; Когато решавате задачите си, можете да го изтриете
 ; или да го промените така, че да проверява условия
 ; свързани с вашия код.
 
 (test-suite
  "member-tree"

  (test-true   "sample test 1" (member-tree? 1 '(1 () ())))
  (test-true   "member in left subtree" (member-tree? 2 '(1 (2 () ()) ())))
  (test-true   "member in right subtree" (member-tree? 3 '(1 () (3 () ()))))
  (test-true   "member in right subtree when left one is not empty" (member-tree? 4 '(1 (2 () ()) (4 () ()))))
  (test-true   "member in deeper left subtree" (member-tree? 3 '(1 (2 (3 () ()) ()) (5 () ()))))

  (test-false  "member not in tree" (member-tree? 5 '(1 (2 (3 () ()) ()) (6 () ()))))
  (test-false  "member not in empty tree" (member-tree? 1 '()))

 )
  
 (test-suite
  "member-tree"

  (test-equal? "sum in empty tree is 0" (sum-tree '()) 0)
  (test-equal? "sample" (sum-tree '(1 () ())) 1)
  (test-equal? "in both subtrees" (sum-tree '(1 (2 () ()) (3 () ()))) 6)
  (test-equal? "for deeper trees" (sum-tree '(1 (2 (3 () ()) ()) (4 () (5 () ())))) 15)

  (test-false  "returns false if tree is not valid" (sum-tree '(1 () (2 (3 () ()) ()) (6 () ()))))

 )

 (test-suite
  "tree-level"

  (test-equal? "empty tree" (tree-level 2 '()) '())
  (test-equal? "simple test" (tree-level 0 '(1 () ())) '(1))
  (test-equal? "level with empty trees" (tree-level 1 '(1 () ())) '())
  (test-equal? "bigger tree" (tree-level 2 '(1 (2 (3 (4 () ()) ()) (5 () ())) (6 () (7 () ())))) '(3 5 7))

  )

 (test-suite
  "map-tree"

  (test-equal? "empty tree" (map-tree (lambda (x) (+ x 1)) '()) '())
  (test-equal? "simple test" (map-tree (lambda (x) (+ x 1)) '(1 (2 () (3 () ())) (5 (6 () ()) ()))) '(2 (3 () (4 () ())) (6 (7 () ()) ())))

  )

  (test-suite
  "tree->list"

  (test-equal? "empty tree" (tree->list '()) '())
  (test-equal? "only root" (tree->list '(1 () ())) '(1))
  (test-equal? "2-level tree" (tree->list '(2 (1 () ()) (3 () ())) ) '(1 2 3))
  (test-equal? "bigger tree" (tree->list '(5 (3 (2 (1 () ()) () ) (4 () ())) (6 () (7 () ()))) ) '(1 2 3 4 5 6 7))
  )

  (test-suite
  "bst-member"

  (test-false "empty tree" (bst-member 2 '()) )
  (test-true "only root" (bst-member 1 '(1 () ())) )
  (test-true "2-level tree" (bst-member 1 '(2 (1 () ()) (3 () ())) ) )
  (test-true "in left-subtree leaf" (bst-member 1 '(5 (3 (2 (1 () ()) () ) (4 () ())) (6 () (7 () ()))) ) )
  (test-true "left-subtree node" (bst-member 2 '(5 (3 (2 (1 () ()) () ) (4 () ())) (6 () (7 () ()))) ) )
  (test-true "right-subtree node" (bst-member 6 '(5 (3 (2 (1 () ()) () ) (4 () ())) (6 () (7 () ()))) ))
  (test-false "biggest number" (bst-member 10 '(5 (3 (2 (1 () ()) () ) (4 () ())) (6 () (7 () ()))) ))
  (test-false "not in left" (bst-member 1 '(5 (3 (2 (0 () ()) () ) (4 () ())) (6 () (8 () ()))) ))
  (test-false "not in right" (bst-member 7 '(5 (3 (2 (0 () ()) () ) (4 () ())) (6 () (8 () ()))) ))

  
  )

  (test-suite
  "bst-add"

  (test-equal? "for empty tree" (bst-add 2 '()) '(2 () ()))
  (test-equal? "go in right subtree" (bst-add 2 '(1 () ())) '(1 () (2 () ())))
  (test-equal? "go in left subtree" (bst-add 2 '(3 () ())) '(3 (2 () ()) ()))
  (test-equal? "for a bigger tree" (bst-add 5 '(10 (3 (2 () ()) (9 (4 () (6 () (7 () (8 () ())))) ())) (11 () ())))  '(10 (3 (2 () ()) (9 (4 () (6 (5 () ()) (7 () (8 () ())))) ())) (11 () ())))

  )
  
  (test-suite
  "sort"

  (test-equal? "empty list" (sort '()) '())
  (test-equal? "simple test" (sort '(3 4 2 5 1)) '(1 2 3 4 5))
  
  )
  
  (test-suite
  "valid-bst?"

  (test-true "empty list" (valid-bst? '()))
  (test-true "simple test" (valid-bst? '(1 () ())) )
  (test-true "bigger tree with subtrees" (valid-bst? '(2 (1 () ()) (3 () (4 () ())))))
  (test-false "not a tree" (valid-bst? '(2 () (1 () ()) (3 () (4 () ())))))
  (test-false "not a bst tree" (valid-bst? '(2 (5 () ()) (3 () (4 () ())))))
  
  
  )

  (test-suite
  "prune"

  (test-equal? "empty list" (prune '(1 () ())) '())
  (test-equal? "simple test" (prune '(1 (2 () (3 () ())) ())) '(1 (2 () ()) ()))
  
  
  )

  (test-suite
  "bloom"

  (test-equal? "just a root" (bloom '(1 () ())) '(1 (1 () ()) (1 () ())))
  (test-equal? "in left subtree" (bloom '(1 (2 () (3 () ())) ())) '(1 (2 () (3 (3 () ()) (3 () ()))) ()))
  (test-equal? "in right subtree" (bloom '(1 () (2 () ()))) '(1 () (2 (2 () ()) (2 () ()))))
  )

  
  )
