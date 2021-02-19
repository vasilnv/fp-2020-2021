replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : (replicate' (n-1) x)


take' :: Int -> [a] -> [a]
take' 0 x = []
take' n [x] = [x]
take' n (x:xs) = x:(take' (n-1) xs)


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [a] -> [(a,a)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) 
    | a == x = True
    | otherwise = elem' a xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
	let smaller = quicksort [a | a <- xs, a <= x]
	    bigger = quicksort [a | a <- xs, a > x]
	in smaller ++ [x] ++ bigger


