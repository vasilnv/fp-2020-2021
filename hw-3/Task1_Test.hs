module Task1_Test where
import Task1
import Test.HUnit

test1 = "inorder works for empty tree" ~:  ([]::[Int]) ~=? (values Inorder EmptyTree)
test2 = "inorder works for a leaf" ~:  [1] ~=? (values Inorder (Node {value = 1, left = EmptyTree, right = EmptyTree}))
test3 = "inorder works for a tree with left subtree" ~: [1,2] ~=? (values Inorder (Node {value = 2, left = Node {value = 1, left = EmptyTree, right = EmptyTree}, right = EmptyTree})) 
test4 = "inorder works for a tree with right subtree" ~:  [1,2] ~=? (values Inorder (Node 1 EmptyTree (Node 2 EmptyTree EmptyTree)))
test5 = "inorder works for a bigger tree" ~:  [1,2,3] ~=? (values Inorder (Node {value = 2, left = Node {value = 1, left = EmptyTree, right = EmptyTree}, right = Node {value = 3, left = EmptyTree, right = EmptyTree}}))

test6 = "preorder works for empty tree" ~: ([]::[Int]) ~=? (values Preorder EmptyTree)
test7 = "preorder works for a leaf" ~: [1] ~=? (values Preorder (Node {value = 1, left = EmptyTree, right = EmptyTree})) 
test8 = "preorder works for a tree with left subtree" ~: [1,2] ~=? (values Preorder (Node {value = 1, left = Node {value = 2, left = EmptyTree, right = EmptyTree}, right = EmptyTree}))
test9 = "preorder works for a tree with right subtree" ~: [1,2] ~=? (values Preorder (Node {value = 1, left = EmptyTree, right = Node {value = 2, left = EmptyTree, right = EmptyTree}}))
test10 = "preorder works for a bigger tree" ~: [1,2,3] ~=? (values Preorder (Node {value = 1, left = Node {value = 2, left = EmptyTree, right = EmptyTree}, right = Node {value = 3, left = EmptyTree, right = EmptyTree}})) 

test11 = "postorder works for empty tree" ~: ([]::[Int]) ~=? (values Postorder EmptyTree) 
test12 = "postorder works for a leaf" ~: [1] ~=? (values Postorder (Node {value = 1, left = EmptyTree, right = EmptyTree}))
test13 = "postorder works for a tree with left subtree" ~:  [1,2] ~=? (values Postorder (Node {value = 2, left = Node {value = 1, left = EmptyTree, right = EmptyTree}, right = EmptyTree}))
test14 = "postorder works for a tree with right subtree" ~:  [1,2] ~=? (values Postorder (Node {value = 2, left = EmptyTree, right = Node {value = 1, left = EmptyTree, right = EmptyTree}}))
test15 = "postorder works for a bigger tree" ~: [1,2,3] ~=? (values Postorder (Node {value = 3, left = Node {value = 1, left = EmptyTree, right = EmptyTree}, right = Node {value = 2, left = EmptyTree, right = EmptyTree}})) 

tl = TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15]


