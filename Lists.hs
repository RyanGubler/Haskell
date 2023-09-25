module Lists where

-- I've left this sample definition in here, you should delete it
countingNumbers :: [Int]
countingNumbers = [1..]

multiplesOfNumbers :: Int -> [Int]
multiplesOfNumbers n = map (* n) [1..]

padovan :: Int -> Int
padovan n
  | n < 3 = 1
  | otherwise = padovan (n - 2) + padovan (n - 3)

padovanNumbers :: [Int]
padovanNumbers = map padovan [0..]

woodallNumbers :: Int -> [Int]
woodallNumbers n = take 10 [n * (2 ^ n) - 1 | n <- [1..]]

order :: (a -> a -> Bool) -> [a] -> [a] -> [a]
order _ [] xs2 = xs2
order _ xs1 [] = xs1
order comparator (x : xs1) (y : xs2) | x `comparator` y = x : order comparator xs1 (y : xs2) | otherwise = y : order comparator (x : xs1) xs2

runLengthEncoding :: Eq a => [a] -> [(a, Int)]
runLengthEncoding = undefined

pairUp :: [a] -> [[a]]
pairUp [] = []
pairUp [x] = [[x]]
pairUp (x1 : x2 : xs) = [x1, x2] : pairUp xs

-- runLengthEncoding :: Eq a => [a] -> [(a, Int)]
-- runLengthEncoding [] = []
-- runLengthEncoding (x:xs) = encodeHelper x 1 xs
--   where
--     encodeHelper :: Eq a => a -> Int -> [a] -> [(a, Int)]
--     encodeHelper current count [] = [(current, count)]
--     encodeHelper current count (y:ys)
--       | current == y = encodeHelper current (count + 1) ys
--       | otherwise = (current, count) : encodeHelper y 1 ys

listPairApply :: [a -> b] -> [[a]] -> [b]
listPairApply _ [] = []  -- Handle the case of an empty input list
listPairApply fs xs = applyFunctions fs xs  -- Start the recursion

applyFunctions :: [a -> b] -> [[a]] -> [b]
applyFunctions _ [] = []  -- Handle the case of an empty list of sublists
applyFunctions [] _ = []  -- Handle the case of an empty list of functions
applyFunctions (f:fs) (sublist:rest) = map (applyFunction f) sublist ++ applyFunctions fs rest
  where
    applyFunction :: (a -> b) -> a -> b
    applyFunction func x = func x

-- Example usage:
-- listPairApply [f1, f2] [[x1, x2], [y1, y2], [w1]] = [f1(x1, x2), f2(y1, y2), f1(w1)]

composeList :: [a -> a] -> (a -> a)
composeList = foldr (.) id





