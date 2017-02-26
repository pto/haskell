-- Exercise 1
double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

average :: [Int] -> Int
average ns = sum ns `div` length ns

a = b + c 
    where
        b = 1
        c = 2
d = a * 2

a' = b' + c' 
    where
        { b' = 1;
          c' = 2 };
d' = a' * 2

a'' = b'' + c'' where { b'' = 1; c'' = 2 }; d'' = a'' * 2 

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
last' xs = head (drop (length xs - 1) xs)

last'' [x] = x
last'' (x:xs) = last'' xs

last''' xs = xs !! (length xs - 1)

last'''' xs = head (reverse xs)

-- Exercise 5
init' xs = take (length xs - 1) xs

init'' [x] = []
init'' (x:xs) = [x] ++ init'' xs

init''' xs = reverse (tail (reverse xs))
