-- Exercise 1
double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

average :: [Int] -> Int
-- average :: Foldable t => t Int -> Int 
average ns = sum ns `div` length ns

a = b + c 
    where
        b = 1
        c = 2
d = a * 2

c = forty + two where { forty = 40; two = 2}; e = c * a

{-
 e = 123 -- This is also a comment
 {- This is a nested comment -- but this is more comment -}
 This would be bad syntax, if it weren't in a comment!
-}

-- Exercise 2
-- (2 ^ 3) * 4
-- (2 * 3) + (4 * 5)
-- 2 + (3 * (4 ^ 5))

-- Exercise 3
n = a `div` length xs
        where
                a  = 10
                xs = [1,2,3,4,5]

-- Exercise 4
myLast1 xs = head (drop (length xs - 1) xs)

myLast2 [x] = x
myLast2 (x:xs) = myLast2 xs

myLast3 xs = xs !! (length xs - 1)

myLast4 xs = head (reverse xs)

-- Exercise 5
myInit1 xs = take (length xs - 1) xs

myInit2 [x] = []
myInit2 (x:xs) = [x] ++ myInit2 xs

myInit3 xs = reverse (tail (reverse xs))
