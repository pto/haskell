-- Exercise 1
-- The recursive factorial function from the text never reaches the base case,
-- overflowing the stack.
fac :: Int -> Int
fac 0         = 1
fac n | n > 0 = n * fac (n - 1)

-- Exercise 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- Exercise 3
(^^^) :: Integer -> Integer -> Integer
infixr 9 ^^^
x ^^^ 0 = 1
x ^^^ n = x * x ^^^ (n - 1)
-- 2 ^^^ 3
-- = { applying ^^^ }
-- 2 * 2 ^^^ 2
-- = { applying ^^^ }
-- 2 * (2 * 2 ^^^ 1)
-- = { applying ^^^ }
-- 2 * (2 * (2 * 2 ^^^ 0))
-- = { applying ^^^ }
-- 2 * (2 * (2 * 1))
-- = { multiplication }
-- 8

-- Exercise 4
euclid :: Int -> Int -> Int
euclid x y | x == y    = x
           | x < y     = euclid x (y - x)
           | x > y     = euclid (x - y) y

-- Exercise 5
-- length [1,2,3]
-- = { applying length }
-- 1 + length [2,3]
-- = { applying length }
-- 1 + (1 + length [3])
-- = { applying length }
-- 1 + (1 + (1 + length []))
-- = { applying length }
-- 1 + (1 + (1 + 0))
-- = { addition }
-- 3
--
-- drop 3 [1,2,3,4,5]
-- = { applying drop }
-- drop 2 [2,3,4,5]
-- = { applying drop }
-- drop 1 [2,3,4,5]
-- = { applying drop }
-- drop 0 [3,4,5]
-- = { applying drop }
-- [3,4,5]
--
-- init [1,2,3]
-- = { applying init }
-- 1 : init [2,3]
-- = { applying init }
-- 1 : (2 : init [3])
-- = { applying init }
-- 1 : (2 : [])
-- = { definition of list }
-- [1,2]

-- Exercise 6
and' :: [Bool] -> Bool
and' []     = True
and' (b:bs) = if b then and' bs else False

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

(!!!) :: [a] -> Int -> a
xs !!! 0      = head xs
(x: xs) !!! n = xs !!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' e (x:xs) = if x == e then True else elem' e xs

-- Exercise 7
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs                     = xs
merge xs []                     = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Exercise 8
halve :: [a] -> ([a], [a])
halve xs = (take count xs, drop count xs)
           where count = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let d = halve xs in
           merge (msort (fst d)) (msort (snd d))

-- Exercise 9
-- Step 1: define the type
-- sum' :: [Int] -> Int
-- Step 2: enumerate the cases
-- sum' []     =
-- sum' (n:ns) =
-- Step 3: define the simple cases
sum' [] = 0
-- Step 4: define the other cases
sum' (n:ns) = n + sum' ns
-- Step 5: generalize and simplify
sum' :: Num a => [a] -> a

-- Step 1: define the type
-- take' :: Int -> [a] -> [a]
-- Step 2: enumerate the cases
-- take' 0 x      =
-- take' n []     =
-- take' n (x:xs) =
-- Step 3: define the simple cases
-- take' 0 x      = []
-- take' n []     = []
-- Step 4: define the other cases
-- take' n (x:xs) = x : take' (n - 1) xs
-- Step 5: generalize and simplify
take' :: Integer -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

-- Step 1: define the type
last' :: [a] -> a
-- Step 2: enumerate the cases
-- last' []     =
-- last' [x]    =
-- last' (x:xs) = 
-- Step 3: define the simple cases
last' []     = error "empty list"
last' [x]    = x
-- Step 4: define the other cases
last' (x:xs) = last xs
-- Step 5: generalize and simplify
-- Nothing needed
