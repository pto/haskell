-- Exercise 1
--      double (double 2)
-- =            { applying the outer double }
--      double 2 + double 2
-- =            { applying the first double }
--      (2 + 2) + double 2
-- =            { applying double }
--      (2 + 2) + (2 + 2)
-- =            { applying the first + }
--      4 + (2 + 2)
-- =            { applying the second + }
--      4 + 4
-- =            { applying + }
--      8

-- Exercise 2
--      sum [x]
-- =            { definition of [x] }
--      sum (x:[])
-- =            { applying sum }
--      x + sum []
-- =            { applying sum }
--      x + 0
-- =            { applying + }
--      x

-- Exercise 3
product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

--      product' [2,3,4]
-- =          { applying product' }
--      2 * product' [3,4]
-- =          { applying product' }
--      2 * (3 * product' [4])
-- =          { applying product' }
--      2 * (3 * (4 * product' []))
-- =          { applying product' }
--      2 * (3 * (4 * 1))
-- =          { applying * }
--      24

-- Exercise 4
reverseSort :: Ord a => [a] -> [a]
reverseSort []     = []
reverseSort (x:xs) = reverseSort larger ++ [x] ++ reverseSort smaller
                     where
                        larger  = [ a | a <- xs, a > x ]
                        smaller = [ a | a <- xs, a <= x ]

-- Exercise 5
-- uniqueSort sorts distinct values in the input list
uniqueSort :: Ord a => [a] -> [a]
uniqueSort []     = []
uniqueSort (x:xs) = uniqueSort smaller ++ [x] ++ uniqueSort larger
                    where
                        smaller = [ a | a <- xs, a < x ]
                        larger  = [ a | a <- xs, a > x ]
