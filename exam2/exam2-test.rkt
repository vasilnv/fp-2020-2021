#lang racket/base

(require "62348.rkt") ; Променете името на файла, който се включва така,
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
  "weight-balanced"

  (test-true "empty tree" (weight-balanced? '()))
  (test-true "simple test" (weight-balanced? '(1 () ())))
  (test-true "higher tree" (weight-balanced? '(1 (2 () ()) (3 () ()))))
  (test-true "balanced with higher left tree " (weight-balanced? '(1 (2 (4 () ()) ()) (3 () ()))))
  (test-true "balanced with higher right tree " (weight-balanced? '(1 (2 () ()) (3 () (4 () ())))))
  (test-false "unbalanced with higher left tree " (weight-balanced? '(1 (2 (4 () (5 () ())) ()) (3 () ()))))
  (test-false "unbalanced in left tree " (weight-balanced? '(1 (2 (4 () (5 (6 () (7 () ())) ())) ()) (3 (1 (4 () ()) ()) (2 (5 () ()) ())))))


 )
  (test-suite
  "attempts"

  (test-equal? "empty" (attempts "d" (list (list 12345 "a" 5)
                    (list 12345 "b" 6)
                    (list 54321 "b" 6))) '())
  (test-equal? "simple case" (attempts "a" (list (list 12345 "a" 5))) (list (cons 12345 1)))
  (test-equal? "bigger list" (attempts "a" (list (list 12345 "a" 5)
                    (list 12345 "b" 6)
                    (list 12345 "b" 6)
                    (list 54321 "a" 5)
                    (list 54321 "b" 6)
                    (list 54321 "a" 5)
                    (list 54321 "b" 6)
                    (list 54321 "c" 6))) (list (cons 12345 1) (cons 54321 2)))



 )



 
)
