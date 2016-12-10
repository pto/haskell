myAdd :: (Int, Int) -> Int
myAdd (a,b) = a + b

zeroto :: Int -> [Int]
zeroto n = [0..n]

add' :: Int -> (Int -> Int)
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myEqual :: Eq a => a -> a -> Bool
myEqual x y = x == y

-- Exercises 1
ex1a = ['a','b','c'] :: [Char]
ex1b = ('a','b','c') :: (Char, Char, Char)
ex1c = [(False,'O'),(True,'1')] :: [(Bool, Char)]
ex1d = ([False,True],['0','1']) :: ([Bool], [Char])
ex1e = [tail,init,reverse] :: [[a] -> [a]]

-- Exercise 2
bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[1,2,3], [4,5,6]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f a = f a

-- Exercise 3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f(f x)

-- Exercise 4
-- (see above)

-- Exercise 5
-- You would have to evaluate both functions for all possible parameter
-- values to show they are equal. When the possible parameters are limited
-- (such as Bool) or when the result does not depend on every possible
-- value (such as thing x = 2), it might be feasible. Or, in some limited
-- cases, you could generate an automated proof that two functions give the
-- same results.
