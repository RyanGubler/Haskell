import Lists

add1 x = 1 + x
add2 x = 2 + x
add3 x = 3 + x

testIt :: (Show t) => (String, t) -> IO ()
testIt (s,f) = do
  putStr "\n"
  putStr s
  putStr "\n"
  print (f)
main = do
  let add1 x = 1 + x
  let add2 x = 2 + x
  mapM_ testIt[
     ("take 3 countingNumbers", take 3 countingNumbers),
     ("take 5 countingNumbers", take 5 countingNumbers),
     ("take 1 (multiplesOfNumbers 5)", take 1 (multiplesOfNumbers 5)),
     ("take 5 (multiplesOfNumbers 2)", take 5 (multiplesOfNumbers 2)),
     ("take 10 padovanNumbers", take 10 padovanNumbers),
     ("take 10 woodallNumbers", take 10 (woodallNumbers 10)),
     ("order (<) (take 5 countingNumbers) (take 5 padovanNumbers)",
      order (<) (take 5 countingNumbers) (take 5 padovanNumbers)),
     ("order (>) [] (take 5 padovanNumbers)",
      order (>) [] (take 5 padovanNumbers)),
     ("order (<) (take 5 woodallNumbers) (take 5 (multiplesOfNumbers 2))",
      order
      (<) (take 5 (woodallNumbers 5)) (take 5 (multiplesOfNumbers 2)))
    ]
  mapM_ testIt [
    ("runLengthEncoding []", runLengthEncoding []),
    ("runLengthEncoding [7]", runLengthEncoding [7]),
    ("runLengthEncoding [7, 7, 4, 7, 7, 7]", 
      runLengthEncoding [7, 7, 4, 7, 7, 7]),
    ("runLengthEncoding (take 5 countingNumbers)", 
     runLengthEncoding (take 5 countingNumbers)),
    ("runLengthEncoding (take 10 padovanNumbers)", 
     runLengthEncoding (take 10 padovanNumbers))
    ]
  mapM_ testIt [
    ("pairUp []", pairUp []),
    ("pairUp (take 3 countingNumbers)",
      pairUp (take 3 countingNumbers)),
    ("pairUp (take 5 countingNumbers)",
      pairUp (take 5 countingNumbers))
    ]
  mapM_ testIt [
    ("listPairApply [(+),(-)] []", 
     show (listPairApply [(+),(-)] [])),
    ("listPairApply [(+),(-)] (pairUp (take 6 countingNumbers))", 
     show (listPairApply [(+),(-)] (pairUp (take 6 countingNumbers)))),
    ("listPairApply [(+),(*)] (pairUp (take 8 countingNumbers))",
     show (listPairApply [(+),(*)] (pairUp (take 8 countingNumbers)))),
     ("listPairApply [(+),(*)] (pairUp (take 7 countingNumbers))",
     show (listPairApply [(+),(*)] (pairUp (take 7 countingNumbers))))
    ]
  mapM_ testIt [
    ("(composeList [add1,add2,add1]) 3", composeList [add1,add2,add1] 3),
    ("(composeList [add1, add2, add3] 3)", composeList [add1,add2,add3] 3),
    ("(composeList [] 3)", composeList [] 3)
    ]
