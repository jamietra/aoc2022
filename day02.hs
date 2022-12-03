scoreChar :: String -> Int
scoreChar (_:_:t:ts)
  | t == 'X' = 1
  | t == 'Y' = 2
  | t == 'Z' = 3
  | otherwise = 0

wins   = ["CX", "AY", "BZ"]
draws  = ["AX", "BY", "CZ"]
losses = ["AZ", "BX", "CY"]

isWin :: String -> Int
isWin (s:_:t:_)
  | elem (s:t:[]) draws = 3
  | elem (s:t:[]) wins = 6
  | otherwise = 0


getScore :: [String] -> Int
getScore s = (sum $ map scoreChar s) + (sum $ map isWin s)


getMove :: String -> String
getMove (m:_:p:_)
  | p == 'X' = m:' ':(getMoveFromChar m losses):[]
  | p == 'Y' = m:' ':(getMoveFromChar m draws):[]
  | p == 'Z' = m:' ':(getMoveFromChar m wins):[]
  where
    getMoveFromChar n ls = head [l !! 1 | l <- ls, (head l) == n]

solve :: String -> Int
solve = getScore . lines

solve2 :: String -> Int
solve2 = getScore . map getMove . lines

main :: IO ()
main = do
  f <- readFile("input/input2.txt")
  print $ solve f
  print $ solve2 f