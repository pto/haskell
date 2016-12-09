double :: Num a => a -> a
double x = x + x

mySum :: Num a => [a] -> a
mySum []     = 0
mySum (x:xs) = x + mySum xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                    smaller = [ a | a <- xs, a <= x ]
                    larger  = [ b | b <- xs, b > x ]

seqn :: Monad m => [m t] -> m [t]
seqn []         = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

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
-- sum [x] = sum (x:[]) = x + sum [] = x + 0 = x

-- Exercise 3
myProduct :: Num a => [a] -> a
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs

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
