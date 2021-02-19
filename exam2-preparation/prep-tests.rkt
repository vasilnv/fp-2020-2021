#lang racket/base

(require "prep.rkt") ; Променете името на файла, който се включва така,
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
  "invert-tree"

  (test-equal? "empty" (invert '()) '())
  (test-equal? "just a root" (invert '(1 () ())) '(1 () ()))
  (test-equal? "left -> right" (invert '(1 (2 () ()) ())) '(1 () (2 () ())))

  (test-equal? "invert works for a bigger tree" (invert '(4 (2 (1 () ()) (3 () ())) (5 () ()))) '(4 (5 () ()) (2 (3 () ()) (1 () ()))))
  (test-false "method returns false if given a list with a different structure" (invert '(1 2 3)))
  
 )

 (test-suite
  "full-tree"
  (test-equal? "empty" (full-tree 0 1) '())
  (test-equal? "1-level" (full-tree 1 1) '(1 () ()))
  (test-equal? "higgher tree" (full-tree 3 2) '(2 (2 (2 () ()) (2 () ())) (2 (2 () ()) (2 () ()))))
  
  
 ) 

 (test-suite
  "extremum"
  (test-equal? "empty" (extremum '()) 0)
  (test-equal? "2 mins" (extremum '((1 2 3) (1 4 5))) 1)
  (test-equal? "2 maxes" (extremum '((1 2 4) (2 4 3))) 4)
  (test-equal? "min-max" (extremum '((1 2 4) (6 4 5))) 4)
  (test-equal? "min-max" (extremum '((1 2 4) (16 14 15))) 0)
  
  
 ) 

 )
