import Test.HUnit

fact 0 = 1
fact n = n * fact (n-1)

test1 = TestCase (do 1 @=? (fact 1))
test2 = 2 ~=? (fact 1)

test3 = "1 = 1!" ~: 1 ~=? (fact 1)