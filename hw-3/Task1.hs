module Task1 where
data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

data Strategy = Inorder | Postorder | Preorder deriving (Show,Read)

values :: Strategy -> (Tree a) -> [a]
values s t = case s of Inorder -> inorder t
                       Postorder -> postorder t
                       Preorder -> preorder t

--values Inorder t = inorder t

inorder :: Tree a -> [a]
inorder EmptyTree = []
inorder (Node val left right) = inorder (left) ++ (val : inorder (right))

preorder :: Tree a -> [a]
preorder EmptyTree = []
preorder (Node val left right) = [val] ++ preorder (left) ++  preorder (right)

postorder :: Tree a -> [a]
postorder EmptyTree = []
postorder (Node val left right) =  postorder (left) ++  postorder (right) ++ [val] 


