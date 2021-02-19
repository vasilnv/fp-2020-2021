--1,2
fibonacci x
   | x <= 0 = error "not a positive number"
   | x == 1 = 1
   | x == 2 = 1
   | otherwise = fibonacci(x - 1) + fibonacci(x-2)

fibonacci1 1 = 1
fibonacci1 2 = 1
fibonacci1 x = fibonacci1 (x-1) + fibonacci1 (x-2)


--3
fastPow _ 0 = 1
fastPow x 1 = x
fastPow x n 
   | even n    = half * half
   | otherwise = half * half * x 
   where half = fastPow x ((div) n 2)


--4
complAdd (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
complSub (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)
complMul (x1,y1) (x2,y2) = (x1 * x2 - y1 * y2, x2 * y1 + x1 * y2)

--5
distance (x1,y1) (x2,y2) = sqrt ((x1-x2) ^ 2 + (y1-y2) ^ 2)

--6 
repeated f 0 x = x
repeated f 1 x = f x
repeated f n x = f (repeated f (n-1) x)
