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
woodallNumbers n = take 10 [n * 2 ^ n - 1 | n <- [1..]]

order :: (a -> a -> Bool) -> [a] -> [a] -> [a]
order compare [] xs2 = xs2
order compare xs1 [] = xs1
order comparator (x : xs1) (y : xs2) | x `comparator` y = x : order comparator xs1 (y : xs2) | otherwise = y : order comparator (x : xs1) xs2

listPairApply :: [a -> a -> a] -> [[a]] -> [a]
listPairApply _ [] = []
listPairApply [] _ = []
listPairApply _ [x]  = x
listPairApply (f:fs) (x:xs) = f (head x) (head (tail x)) : listPairApply (fs ++ [f]) xs

pairUp :: [a] -> [[a]]
pairUp [] = []
pairUp [x] = [[x]]
pairUp (x1 : x2 : xs) = [x1, x2] : pairUp xs

runLengthEncoding :: Eq a => [a] -> [(a, Int)]
runLengthEncoding [] = []
runLengthEncoding (x:xs) = runLengthEncoding' xs x 1
  where
    runLengthEncoding' :: Eq a => [a] -> a -> Int -> [(a, Int)]
    runLengthEncoding' [] current count = [(current, count)]
    runLengthEncoding' (y:ys) current count
      | y == current = runLengthEncoding' ys current (count + 1)
      | otherwise = (current, count) : runLengthEncoding' ys y 1

composeList :: [a -> a] -> (a -> a)
composeList = foldr (.) id



