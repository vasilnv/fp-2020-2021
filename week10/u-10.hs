--zipWith (+) [1,2,3] [4,5,6]
--zip [0..] "gosho"
--map (\x -> x * x) [1..10]
--[ (x,y) | x <- [1,2,3], y <- [4,5,6]]

--2
countDivisors n =  length [d | d <- [1..n-1], mod n d == 0]
decartes lst1 lst2 = [(x,y) | x <- lst1, y <- lst2]

primes n 
   | n == 1 = True
   | countDivisors n == 1 = True
   | otherwise = False

prime1 n = (countDivisors n) == 1

--3
primesRes = [x | x <- [2..], countDivisors x == 1]


--4
--sieve(x:xs) = x : sieve (filter (\y -> mod y x /= 0) xs)
primes' = sieve [2..]
   where sieve(x:xs) = x : sieve [y | y<-xs, mod y x /= 0]


---

reverse' lst = foldl (\ res el -> (el:res)) [] lst
---

--5
pairs = [ (d-i,i) | d<-[0..], i<-[0..d] ]

--6
pyths = [ (a,b,c) | c<-[5..], b<-[1..c-1], a<-[1..b-1], a^2 + b^2 == c^2 ]

--7
compress [] = []
compress lst = (head lst, length heads) : compress rest
   where (heads, rest) = span (\x -> x == head lst) lst


--8
maxRepeated [] = 0
maxRepeated l = foldr (\ (_,x) res -> if x > res then x else res) 0 compressed
   where compressed = compress l


 --9
makeSet [] = []
makeSet l = foldr (\ x res -> if elem x res then res else x:res) [] l


--10
histogram lst = [ (el,count el) | el<-makeSet lst ]
   where count el = length [ x | x<-lst, x==el ]

-- 11
maxDistance pts = maximum [ dist p1 p2 | p1<-pts, p2<-pts ]
   where dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)


