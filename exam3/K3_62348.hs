module K3_62348 where



----1

data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)


treeWords :: Tree Char -> [String]
treeWords EmptyTree = []
treeWords (Node a left right) = (helper left [a]) ++ (helper right [a])


helper :: Tree Char -> String -> [String]
helper (Node a EmptyTree right) path = [(generateStringRight right (path++ [a])) ]
helper (Node a left EmptyTree) path = [(generateStringLeft left (path ++ [a]))]
helper (Node a left right) path = (helper left (path ++ [a])) ++ (helper right (path ++ [a]))  

generateStringLeft :: Tree Char -> String -> String
generateStringLeft (Node a EmptyTree _) path = path ++ [a]
generateStringLeft (Node a left _) path = (generateStringLeft left (path ++ [a])) 
generateStringLeft EmptyTree  path = path

generateStringRight :: Tree Char -> String -> String
generateStringRight (Node a _ EmptyTree) path = path ++ [a]
generateStringRight (Node a _ right) path = (generateStringRight right (path ++ [a])) 
generateStringRight EmptyTree path = path



---2
isInjective :: Integral t => (t -> t) -> t -> t -> Bool
isInjective f a b = null $ (\ list -> list \\ (nub list)) (map f [a..b])

