-- C U R R I E D    F U N C T I O N S

--(Ord a) => a -> a -> a can also be written as max :: (Ord a) => a -> (a -> a). That could be read as: max takes an a and returns (that's the ->) a function that takes an a and returns an a.


compareWithHundred x = compare 100 x

applyTwice :: (a->a)->a->a
applyTwice f x = f (f x)


zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : (zipWith' f xs ys)

flip' :: (a->b->c) -> b->a->c
flip' f x y = f y x 


map' _ [] = []
map' f (x:xs) = (f x) : (map f xs)

filter' p [] = []
filter' p (x:xs) 
    | p x = x : filter' p xs
    | otherwise = filter' p xs


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    let smaller = quickSort (filter (<= x) xs)
        bigger = quickSort (filter (> x) xs)
    in smaller ++ [x] ++ bigger


-- find largest number under 100 000 thats divisible by 3829

largestDivisible = (head (filter (\x -> mod x 3829 == 0) [100000,99999..]))

chain :: (Integral a) => a -> [a]  
chain 1 = [1]
chain n 
    | even n = n : chain (div n 2)
    | otherwise = n : chain(n * 3 + 1)


sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

maximum'::(Ord a) => [a] -> a
maximum' = foldl1 (\acc x -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) [] 

product' :: (Num a) => [a] -> a
product' = foldl (*) 1


filter_ :: (a->Bool) -> [a] -> [a]
filter_ p = foldr (\x acc -> if p x then x:acc else acc) []


head' :: [a] -> a
head' = foldr1 (\x acc -> x)

last' :: [a] -> a
last' = foldl1 (\acc x -> x)