#lang racket/base
(require rackunit rackunit/gui)
(require "tree.rkt")


(define tree-unordered
  (make-tree 7
             (make-tree 3
                        (make-leaf 2)
                        '())
             (make-tree 10
                        (make-leaf 8)
                        (make-leaf 9))
             ))

(define tree-ordered-balanced
  (make-tree 7
             (make-tree 3
                        (make-leaf 2)
                        (make-tree 5
                                   (make-leaf 4)
                                   (make-leaf 6)))
             (make-tree 10
                        (make-leaf 9)
                        (make-leaf 11))
             ))

(define tree-ordered-unbalanced
  (make-tree 7
             (make-tree 3
                        (make-leaf 2)
                        (make-tree 5
                                   (make-leaf 4)
                                   (make-leaf 6)))
             (make-leaf 10)
             ))

(define tree-ordered-unbalanced1
  (make-tree 21
             (make-tree 15
                        (make-leaf 2)
                        (make-tree 17
                                   (make-leaf 16)
                                   (make-tree 19
                                              (make-leaf 18)
                                              '())))
             (make-tree 30
                        (make-tree 25
                                   (make-leaf 22)
                                   (make-leaf 27))
                        (make-tree 100
                                   (make-leaf 90)
                                   (make-leaf 120)))
             ))


(test/gui
 (test-suite
 "tree?"

 (test-true "Evaluates an empty tree correctly" (tree? "*"))
 (test-true "Works for a leaf" (tree? "{1**}"))
 (test-true "Works with spaces" (tree? "{1 * * }"))
 (test-true "Works fine with a bigger tree" (tree? "{1{2**}{4{3**}*}}"))
 (test-true "Strings with a lot of spaces are evaluated correctly" (tree? "   {1      {2     * * } {   4 {     3 *   *}         *} }    "))
 (test-true "Test for a two-digit number" (tree? "{12 * * }"))
 (test-false "Returns false if tree has two numbers separated with whitespace" (tree? "{12 2 * * }"))
 (test-false "Returns false with more numbers" (tree? "{1 2 {2 * *}}"))
 (test-true "Check with a left subtree" (tree? "{1 {2 * * } * }"))
 (test-true "Check with a right subtree" (tree? "{1 * {2 * * }  }"))
 (test-true "Check with both left and right subtrees" (tree? "{1 {3 * {1 {2 * * } {0 * *}}} * }"))
 (test-false "Returns false if the string does not start with a {" (tree? "1 * { 1 * *}}"))
 (test-false "Returns false if we are using different scopes" (tree? "[ * [ 1 * *}}"))
 (test-false "Check with a leaf that does not have a root" (tree? "{1 {* * *} 2}"))
 (test-false "Check with an empty tree as root" (tree? "{* { 1 * *}}"))
 (test-true "Strings with spaces in the beginning are evaluated correctly" (tree? "    {1 * * }"))
 (test-true "Strings with spaces in the end are evaluated correctly" (tree? "{1 * * }     "))
 (test-false "Returns false when elements are not put as expected " (tree? "{1 2 3}"))
 (test-false "Returns false when given a letter instead of number" (tree? "{1 {a * *} *}"))
 (test-false "Returns false when string starts with *" (tree? "* {1 {1 * *} *}"))
 (test-false "Returns false when string is just a number" (tree? "21"))
 (test-false "Returns false when string has length 1 and is not an empty tree" (tree? "1"))
 (test-false "Returns false for an empty set" (tree? "{}"))



 )

 (test-suite
  "tree->string"
  (test-case "Empty tree is evaluated correctly"
             (check-equal? (tree->string '()) "*"))
  
  (test-case "Trees are evaluated correctly"
             (check-equal? (tree->string (quote (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))) "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
             (check-equal? (tree->string tree-unordered) "{7 {3 {2 * *} *} {10 {8 * *} {9 * *}}}")
             (check-equal? (tree->string '(2 (4 () ()) ())) "{2 {4 * *} *}")
             (check-equal? (tree->string '(2 () (2 () ()))) "{2 * {2 * *}}")
             (check-equal? (tree->string '(2 (4 () ()) ())) "{2 {4 * *} *}")
             
             )
  (test-case "Method returns false if the given tree is not correct" 
    (check-false (tree->string (quote (5 2 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))))
    (check-false (tree->string (quote (5 (22 (2 1 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))))
    (check-false (tree->string (quote (1 2 3))))
   )

  )

 (test-suite
  "ordered?"

  (test-true "Empty tree is checked as ordered " (ordered? '()))
  (test-case "Tree is checked as ordered correctly"
             (check-true (ordered? tree-ordered-balanced))
             (check-true (ordered? tree-ordered-unbalanced))
             (check-true (ordered? tree-ordered-unbalanced1))
             (check-true (ordered? '(2 () ())))
             (check-true (ordered? '(2 (1 () ()) (3 () ())))))

  (test-case "Tree is checked as unordered correctly"
             (check-false (ordered? tree-unordered))
             (check-false (ordered? '(2 (3 () ()) ())))
             (check-false (ordered? '(3 () (2 () ()))))
  
  ))

 (test-suite
  "balanced?"
  (test-true "Empty tree is checked as balanced correctly"
             (balanced? '()))
  
  (test-case "Tree is checked as balanced correctly"
             (check-true (balanced? tree-ordered-balanced))
             (check-true (balanced? tree-unordered))
             (check-true (balanced? '(2 () ())))
             (check-true (balanced? '(2 (4 () ()) ())))
             (check-true (balanced? '(2 () (4 () ()))))
             (check-true (balanced? '(2 (3 () ()) (4 () ()))))
             
             )
             
             
  (test-case "Tree is checked as unbalanced correctly"
             (check-false (balanced? tree-ordered-unbalanced))
             (check-false (balanced? tree-ordered-unbalanced1))
             (check-false (balanced? '(2 (3 (4 (5 () ()) ()) ()) ())))
             (check-false (balanced? '(2 (3 (4 () ()) ()) ())))
             (check-false (balanced? '(2 () (4 (3 () ()) ()))))
             (check-false (balanced? '(2 () (4 () (3 () ()) ()))))

             )

  )

 
  

 
)