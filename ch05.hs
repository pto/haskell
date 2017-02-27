import Data.Char

-- Exercise 1
sumOfSquares :: Int
sumOfSquares = sum [x*x | x <- [1..100]]

-- Exercise 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- Exercise 3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- Exercise 4
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- Exercise 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

-- Exercise 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

-- Exercise 7
goal :: [(Int, Int)]
goal = [(x, y) | x <- [1,2], y <- [3,4]]

solution :: [(Int, Int)]
solution = concat [[(x, y) | y <- [3,4]] | x <- [1,2]]

-- Exercise 8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..])

-- Exercise 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- Exercise 10
let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | isUpper c = ord c - ord 'A'

int2lower :: Int -> Char
int2lower n = chr (ord 'a' + n)

int2upper :: Int -> Char
int2upper n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2lower ((let2int c + n) `mod` 26)
          | isUpper c = int2upper ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

count :: Char -> String -> Int
count x xs = length [x' | x' <- map toLower xs, toLower x == x']

alphas :: String -> Int
alphas xs = length [x | x <- xs, x >= 'a' && x <= 'z' ||
                                 x >= 'A' && x <= 'Z']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = alphas xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
           where factor = head (positions (minimum chitab) chitab)
                 chitab = [chisqr (rotate n table') table | n <- [0..26]]
                 table' = freqs xs

