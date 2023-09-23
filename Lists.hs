module Lists where
module Lists where

  -- I've left this sample definition in here, you should delete it
  countingNumbers ::[Int]
  countingNumbers = [1..]
  
  multiplesOfNumbers :: Int -> [Int]
  multiplesOfNumbers n = map (* n) [1..]
  
  padovanNumbers :: [Int]
  padovanNumbers = [0]

  woodallNumbers :: [Int]
  woodallNumbers = [0]

  order :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  order _ [] xs = xs
  order _ xs [] = xs
  order cmp (x:xs) (y:ys)
    | cmp x y   = x : order cmp xs (y:ys)
    | otherwise = y : order cmp (x:xs) ys
  
  runLengthEncoding :: Eq a => [a] -> [(a, Int)]
  runLengthEncoding = undefined
  
  pairUp :: [a] -> [(a, a)]
  pairUp = undefined
  
  listPairApply :: [a -> a -> a] -> [a] -> [a]
  listPairApply = undefined

  composeList :: [a -> a] -> a -> a
  composeList [] x = x
  composeList (f:fs) x = composeList fs (f x)

  




