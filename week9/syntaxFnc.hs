head'::[a]->a
head' [] = error "can't call on empty list"
head' (x:xs) = x

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' (xs)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum (xs)

--Guards

bmiTell :: (RealFloat a) => a -> a -> String

bmiTell weight height
   | bmi <= 18.5 = "underweight"
   | bmi <= 25.0 = "normal"
   | bmi <= 30.0 = "fat"
   | otherwise = "whale"
   where bmi = weight / height ^ 2


calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
   where bmi height weight = weight / height ^ 2 

