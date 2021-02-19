module First where
x::Integer
x = 2

y = fromInteger x^2 + 7.5

my_fnc a b = a^b

square::Int->Int
square x = x * x


hypothenuse :: Double -> Double -> Double
hypothenuse a b = sqrt (a^2 + b^2)

div50 = div 50

twice f x = f (f x)
--twice sqrt 81

diag f x = f x x
--doub = diag (^)

fact x
   | x == 0 = 1
   | x > 0 = x * fact (x - 1)
   | x < 0 = error "Negative number"

-- отложено оценяване!

fib x
   | x == 1 = 1
   | x == 2 = 1
   | x < 1 = error "Invalid args"
   | otherwise = fib (x-1) + fib (x-2)

sum' [] = 0
sum' (x:xs) = x + sum' xs
