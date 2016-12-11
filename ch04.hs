import Data.List (intercalate)

-- Fizzbuzz
fizzBuzz :: IO ()
fizzBuzz = let fizzBuzzLine x | x `mod` 15 == 0 = "FizzBuzz"
                              | x `mod` 3 == 0  = "Fizz"
                              | x `mod` 5 == 0  = "Buzz"
                              | otherwise       = show x
               fizzBuzzList = map fizzBuzzLine [1..100]
           in  putStr (unlines fizzBuzzList)
        
-- Triangle
nextValue :: [[Int]] -> Int
nextValue xss = last (last xss) + 1

line :: Int -> Int -> [Int]
line n size = [n..n + size - 1]

tri :: Int -> [[Int]]
tri 1 = [[1]]
tri n = tri (n - 1) ++ [line (nextValue (tri (n - 1))) n]

triangle :: Int -> IO ()
triangle n = let lineToString xs = intercalate " " (map show xs)
                 triToString = unlines (map lineToString (tri n))
             in putStr triToString

-- Exercise 1
halve :: [a] -> ([a], [a])
halve xs | even (length xs) = let half = length xs `div` 2
                                in (take half xs, drop half xs)
         | otherwise        = error "only works on even length lists"

-- Exercise 2
third_a :: [a] -> a
third_a xs = head (tail (tail xs))

third_b :: [a] -> a
third_b xs = xs !! 2

third_c :: [a] -> a
third_c (_:_:x:_) = x

-- Exercise 3
safetail_a :: [a] -> [a]
safetail_a xs = if null xs then [] else tail xs

safetail_b :: [a] -> [a]
safetail_b xs | null xs   = []
              | otherwise = tail xs

safetail_c :: [a] -> [a]
safetail_c (_:xs) = xs
safetail_c _      = []

-- Exercise 4
or1 :: Bool -> Bool -> Bool
or1 False False = False
or1 True True   = True
or1 True False  = True
or1 False True  = True

or2 :: Bool -> Bool -> Bool
or2 False False = False
or2 _ _         = True

or3 :: Bool -> Bool -> Bool
or3 True _ = True
or3 False b = b

or4 :: Bool -> Bool -> Bool
or4 a b | a == b    = a
        | otherwise = True

-- Exercise 5
and_if_1 :: Bool -> Bool -> Bool
and_if_1 a b = if a then if b then True
                              else False
                    else False

-- Exercise 6
and_if_2 :: Bool -> Bool -> Bool
and_if_2 a b = if a then b
                    else False

-- Exercise 7
mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z
mult = \x -> (\y -> (\z -> x*y*z))

-- Exercise 8
luhnDouble :: Int -> Int
luhnDouble d = if double > 9 then double - 9
                             else double
               where double = d + d

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = let calc = luhnDouble a + b + luhnDouble c + d
                    in calc `mod` 10 == 0

