double x = x + x

mySum [] = 0
mySum (x:xs) = x + mySum xs

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [ a | a <- xs, a <= x ]
                 larger = [ b | b <- xs, b > x ]

-- Exercise 3
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

-- Exercise 4
reverseSort [] = []
reverseSort (x:xs) = reverseSort larger ++ [x] ++ reverseSort smaller
                                                where
                                                        larger = [ a | a <- xs, a > x ]
                                                        smaller = [ a | a <- xs, a <= x ]

-- Exercise 5
setSort [] = []
setSort (x:xs) = setSort smaller ++ [x] ++ setSort larger
                                        where
                                                smaller = [ a | a <- xs, a < x ]
                                                larger = [ a | a <- xs, a > x ]
