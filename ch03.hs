add :: (Int, Int) -> Int
add (a,b) = a + b

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

-- Exercise 2
second xs = head (tail xs)
swap (x,y) = (y, x)
pair x y = (x, y)
double x = x * 2
palindrome xs = reverse xs == xs
twice f x = f(f x)
