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
-- Using the definition in the book (equal results for equal arguments) you 
-- could evaluate both functions for all possible parameter values to see if
-- they are equal. This would be feasible when the possible parameters are
-- limited (such as Bool) or when the result does not depend on every possible
-- value (such as thing x = 2), but not in the general case. In some cases,
-- you could use textual transformations on the source to prove that two
-- functions are effectively the same, but the absence of a transformation
-- would not prove the functions unequal.
