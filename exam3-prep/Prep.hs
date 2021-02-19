module Prep where

import Data.List


isPalyndrome n = (toList n) == (reverse (toList n))


toList :: Int -> [Int]
toList 0 = []
toList n = rem n 10 : (toList $ div n  10)

freq l = helper l []

helper [] _ = []
helper l set 
    | elem (head l) set = helper (tail l) set
    | otherwise = ((head l), tFound) : (helper (tail l) ((head l):set))
    where tFound = length (filter (== (head l)) l)

----------------------



something epsilon x i 
    | res < epsilon = res
    | otherwise = res + (something epsilon x (i + 1))
    where res = x / (sum (take i [1,2..]))


checkIfMonotonne :: [(Int,Int)] -> Bool
checkIfMonotonne ((x,y):xs) = helper1 xs y

helper1 :: [(Int,Int)] -> Int -> Bool
helper1 [] _ = True
helper1 ((y,x):xs) prev 
    | x > prev = helper1 xs x
    | otherwise = False


makeMonotonne ((x,y):xs) = (x,y) :( helper2 xs y )

helper2 [] _ = []
helper2 ((y,x):xs) prev 
    | x > prev = (y,x) : (helper2 xs x)
    | otherwise = helper2 xs prev



------------------------------------------------------
-- sumLast init n - безкраен поток, който започва с init, а всеки следващ елемент е сума на последните n от потока
-- sumLast 3 5 → [3, 3, 6, 12, 24, 48, 93, 183, ... ]

sumLast::Int -> Int -> [Int]
sumLast init' n = [init', init'] ++ generate [init', init']
    where generate memory = sum memory : generate (sum memory : (if length memory >= n  then (init memory) else memory))

-- Да се напише функция transformSum, която преобразува дърво с елементи цели числа в ново дърво със същата структура,
-- в което всеки елемент е заменен със сумата на елементите в поддървото с този корен в началното дърво.

data Tree1 t = EmptyTree1 | Node1 {root :: Int 
                                , left :: Tree1 t
                                , right:: Tree1 t} deriving (Show, Read)


subTreeSum :: Tree1 Int -> Int
subTreeSum EmptyTree1 = 0
subTreeSum (Node1 root left right) = root + subTreeSum left + subTreeSum right

transformSum :: Tree1 Int -> Tree1 Int
transformSum EmptyTree1 = EmptyTree1
transformSum n = Node1 (subTreeSum n) (transformSum (left n)) (transformSum (right n))



-- Да се генерират всички подсписъци на даден такъв
-- [1,2,3] -> [[1,2,3],[1,2],[2,3],[1],[2],[3]]

--[1,2,3] -> [1] : [2,3] : subseq [[2,3]] -> [[1], [2,3], [2], [3]]
--subsequences :: [a] -> [[a]]
subsequences1 l = nub(res ++ (reverse (helper10 (reverse l) res)))
    where res = helper10 l [l]

helper10 [x] set = if (elem [x] set) then set else [x]:set 
helper10 (x:xs) set
    | (elem [x] set) && (elem xs set) = helper10 xs set
    | (not (elem [x] set)) && (not (elem xs set)) = helper10 xs ([x] : xs : set)
    | (elem [x] set) && (not (elem xs set)) = helper10 xs (xs : set)
    | (not (elem [x] set)) && (elem xs set) = helper10 xs ([x] : set)


subsequences' :: [a] -> [[a]]
subsequences' [] = []
subsequences' xs = [suffix | prefixes <- inits xs, suffix <- tails prefixes]
    where inits [] = []
          inits xs = xs : inits (init xs)
          tails [] = []
          tails xs = xs : tails (tail xs)


-- Да се генерира поток sumsOfCubes от тези числа, които са сума от кубовете на две положителни цели числа
sumsOfCubes = [x^3 + y^3 | x <- [1,2..], y <- [1,2..]]



--------------------------------------------------------
-- Zippers - Learn You a Haskell

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        ) 


data Direction = L | R deriving (Show)

changeToP :: [Direction] -> Tree Char -> Tree Char
changeToP (L:ds) (Node root left right) = Node root (changeToP ds left) right
changeToP (R:ds) (Node root left right) = Node root left (changeToP ds right)
changeToP [] (Node _ left right) = Node 'P' left right


elemAt :: [Direction] -> Tree a -> a  
elemAt (L:ds) (Node _ l _) = elemAt ds l  
elemAt (R:ds) (Node _ _ r) = elemAt ds r  
elemAt [] (Node x _ _) = x 






type ListZipper a = ([a],[a]) 

goForward :: ListZipper a -> ListZipper a  
goForward (x:xs, bs) = (xs, x:bs) 


goBack :: ListZipper a -> ListZipper a  
goBack (xs, b:bs) = (b:xs, bs)  



-----------------


data Tree' a = EmptyTree' | Node' {
                            value1 :: a,
                            left1  :: Tree' a,
                            right1 :: Tree' a
                          } deriving (Show,Read)

treeInsert::Int -> Tree' Int -> Tree' Int
treeInsert x EmptyTree' = Node' x EmptyTree' EmptyTree'
treeInsert x (Node' a left right)
    | x == a = Node' a left right
    | x < a = Node' a (treeInsert x left) right
    | x > a = Node' a left (treeInsert x right)

