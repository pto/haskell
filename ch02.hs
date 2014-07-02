double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]

average xs = sum xs `div` length xs

a = b + c
        where
                b = 1
                c = 2

d = a * 2

c = fortyTwo where { fortyTwo = 42 }

-- This is a comment

{-
 e = 123
-}

-- Exercise 3
n = a `div` length xs
        where
                a = 10
                xs = [1,2,3,4,5]

-- Exercise 4
myLast1 xs = head (drop (length xs - 1) xs)

myLast2 [x] = x
myLast2 (x:xs) = myLast2 xs

myLast3 xs = xs !! (length xs - 1)

-- Exercise 5
myInit1 xs = take (length xs - 1) xs

myInit2 [x] = []
myInit2 (x:xs) = [x] ++ myInit2 xs
