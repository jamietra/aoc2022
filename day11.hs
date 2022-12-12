import Data.List

data Monkey = Monkey { mID :: Int
                     , items :: [Int]
                     , operation :: Int -> Int
                     , test :: Int -> Bool
                     , trueDest :: Int
                     , falseDest :: Int 
                     , inspectionCount :: Int}

postInspection :: Int -> Int
postInspection = floor . (*) (1/3) . fromIntegral

divisibleTest :: Int -> Int -> Bool
divisibleTest t x = x `mod` t == 0

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split c xs = fst spanned : split c (snd spanned)
    where
    noC = dropWhile (==c) xs
    spanned = span (/=c) noC

readWithComma :: String -> Int
readWithComma s = read $ delete ',' s

parseOperation :: [String] -> (Int -> Int)
parseOperation ["old", "*", "old"] = postInspection . flip (^) 2
parseOperation [_, "*", s]         = postInspection . (*) (read s :: Int)
parseOperation [_, "+", s]         = postInspection . (+) (read s :: Int)
parseOperation s = error $ show s

parseOperation2 :: Int -> [String] -> (Int -> Int)
parseOperation2 n ["old", "*", "old"] = flip mod n . flip (^) 2
parseOperation2 n [_, "*", s]         = flip mod n . (*) (read s :: Int) 
parseOperation2 n [_, "+", s]         = flip mod n . (+) (read s :: Int)
parseOperation2 n s = error $ show s

parseMonkey :: ([String] -> (Int -> Int)) -> [String] -> Monkey
parseMonkey p s = Monkey 
    { mID = read (head $ split ':' $ last $ words $ head s) :: Int
    , items = map readWithComma $ words $ last (split ':' (s !! 1))
    , operation = p (last $ split "=" $ words $ last $ split ':' (s !! 2))
    , test = divisibleTest (read $ last $ words (s !! 3) :: Int)
    , trueDest = read (last $ words (s !! 4)) :: Int
    , falseDest = read (last $ words (s !! 5)) :: Int 
    , inspectionCount = 0}

numToDivBy :: [String] -> Int
numToDivBy s = read $ last $ words (s !! 3) :: Int

numToModulo :: String -> Int
numToModulo = product . map numToDivBy . split "" . lines

parseInput1 :: String -> [Monkey]
parseInput1 = map (parseMonkey parseOperation) . split "" . lines

parseInput2 :: String -> [Monkey]
parseInput2 s = map (parseMonkey (parseOperation2 n)) $ split "" $ lines s
    where
        n = numToModulo s

processItem :: Monkey -> Int -> (Int, Int)
processItem m x = (dest, newItem)
    where    
        newItem = operation m x
        dest = if test m newItem then trueDest m else falseDest m 

processMonkey :: Monkey -> (Monkey, [(Int, Int)])
processMonkey m = (m {items = [], inspectionCount = newCount}, map (processItem m) (items m))
    where newCount = inspectionCount m + length (items m)

sendItem :: [Monkey] -> (Int, Int) -> [Monkey]
sendItem ms i = [addItem m i| m <- ms]
    where 
        addItem monk item 
            | fst item == mID monk = monk {items = items monk ++ [snd item]}
            | otherwise = monk

sendItems :: [Monkey] -> [(Int, Int)] -> [Monkey]
sendItems = foldl sendItem

oneStep :: [Monkey] -> Monkey -> [Monkey]
oneStep ms singlem = sendItems updatedms itemsToSend
    where 
        (newm, itemsToSend) = processMonkey singlem
        updatedms = sortOn mID [if mID m == mID singlem then newm else m | m <- ms]

loopRound :: [Monkey] -> Int -> [Monkey]
loopRound ms i 
    | i + 1 == length ms = newms
    | otherwise = loopRound newms (i + 1)
    where newms = oneStep ms (ms !! i)

processRound :: [Monkey] -> [Monkey]
processRound ms = loopRound ms 0

showMonkey :: Monkey -> String
showMonkey m = unwords [show (mID m), show (items m), show (inspectionCount m)]

showMonkeys :: [Monkey] -> String
showMonkeys = unlines . map showMonkey

printMonkeys :: [Monkey] -> IO ()
printMonkeys = putStr . showMonkeys

iterateRounds :: Int -> [Monkey] -> [Monkey]
iterateRounds i ms = iterate processRound ms !! i

solve1 :: String -> Int
solve1 = product . take 2 . reverse . sort . map inspectionCount . iterateRounds 20 . parseInput1

solve2 :: String -> Int
solve2 = product . take 2 . reverse . sort . map inspectionCount . iterateRounds 10_000 . parseInput2

main :: IO ()
main = do
    f <- readFile "input/input11.txt"
    print $ solve1 f
    print $ solve2 f

