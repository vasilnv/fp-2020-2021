module Prep_Test where

import Test.HUnit
import Prep   -- Променете името на модула, който се включва така,
            -- че тук да се зарежда модулът с вашето решение.
            -- Например ако модулът с решението ви се казва K3_12345,
            -- променете реда на import K3_12345
            -- (Можете да изтриете този коментар след като направите промените)

-- Даденият по-долу пример е само ориентировъчен.
-- Когато решавате задачите си, можете да го изтриете
-- или да го промените така, че да проверява условия
-- свързани с вашия код.

-- sumLast init n - безкраен поток, който започва с init, а всеки следващ елемент е сума на последните n от потока
-- sumLast 3 5 → [3, 3, 6, 12, 24, 48, 93, 183, ... ]
test1 = "sumLast works" ~: [3,3,6,12,24,48,93] ~=? take 7 (sumLast 3 5)

--test2 = "transformSum works" ~: Tree 5 (Tree 10 EmptyTree EmptyTree) (Tree 3 EmptyTree EmptyTree) ~=? Tree 18 (Tree 10 EmptyTree EmptyTree) (Tree 3 EmptyTree EmptyTree) 


tl1 = TestList [test1]

main = do
  runTestTT tl1
