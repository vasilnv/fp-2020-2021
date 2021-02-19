module Streams where
-- 1 2 3 4 5 6 7 8 9 10 11
-- 1 2 2 3 3 4 4 4 5 5  5 

seq' = 1:2:2:(genSeq 2 3)

genSeq :: Int -> Int -> [Int]
genSeq 0 n = genSeq (seq'!!n) (n+1)  
genSeq i n = n:genSeq (i-1) n

-- sumLast init n - безкраен поток, който започва с init, а всеки следващ елемент е сума на последните n от потока
-- sumLast 3 5 → [3, 3, 6, 12, 24, 48, 93, 183, ... ]

sumLast init n = [init, init] ++ genNext [init, init] n

genNext currSet n 
     | length currSet < n = (sum currSet) : (genNext (currSet ++ [sum currSet])  n)
     | otherwise = (sum (currSet)) : (genNext (tail currSet ++ [(sum (currSet))] ) n)