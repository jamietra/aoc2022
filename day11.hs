{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Text.Parsec (parse, oneOf)
import Data.List

data Monkey = Monkey { mID :: Integer
                     , items :: [Integer]
                     , operation :: Integer -> Integer
                     , test :: Integer -> Bool
                     , trueDest :: Integer
                     , falseDest :: Integer 
                     , inspectionCount :: Int}

postInspection :: Integer -> Integer
postInspection = floor . (*) (1/3) . fromIntegral

divisibleTest :: Integer -> Integer -> Bool
divisibleTest t x = x `mod` t == 0

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split c xs = fst spanned : split c (snd spanned)
    where
    noC = dropWhile (==c) xs
    spanned = span (/=c) noC

readWithComma :: String -> Integer
readWithComma s = read $ delete ',' s

parseOperation :: [String] -> (Integer -> Integer)
parseOperation ["old", "*", "old"] = postInspection . flip (^) 2
parseOperation [_, "*", s]         = postInspection . (*) (read s :: Integer)
parseOperation [_, "+", s]         = postInspection . (+) (read s :: Integer)
parseOperation s = error $ show s

parseOperation2 :: [String] -> (Integer -> Integer)
parseOperation2 ["old", "*", "old"] = flip (^) 2
parseOperation2 [_, "*", s]         = (*) (read s :: Integer)
parseOperation2 [_, "+", s]         = (+) (read s :: Integer)
parseOperation2 s = error $ show s

parseMonkey :: ([String] -> (Integer -> Integer)) -> [String] -> Monkey
parseMonkey p s = Monkey 
    { mID = read (head $ split ':' $ last $ words $ head s) :: Integer
    , items = map readWithComma $ words $ last (split ':' (s !! 1))
    , operation = p (last $ split "=" $ words $ last $ split ':' (s !! 2))
    , test = divisibleTest (read $ last $ words (s !! 3) :: Integer)
    , trueDest = read (last $ words (s !! 4)) :: Integer
    , falseDest = read (last $ words (s !! 5)) :: Integer 
    , inspectionCount = 0}

parseInput1 :: String -> [Monkey]
parseInput1 = map (parseMonkey parseOperation) . split "" . lines

parseInput2 :: String -> [Monkey]
parseInput2 = map (parseMonkey parseOperation2) . split "" . lines

processItem :: Monkey -> Integer -> (Integer, Integer)
processItem m x = (dest, newItem)
    where    
        newItem = operation m x
        dest = if test m newItem then trueDest m else falseDest m 

processMonkey :: Monkey -> (Monkey, [(Integer, Integer)])
processMonkey m = (m {items = [], inspectionCount = newCount}, map (processItem m) (items m))
    where newCount = inspectionCount m + length (items m)

sendItem :: [Monkey] -> (Integer, Integer) -> [Monkey]
sendItem ms i = [addItem m i| m <- ms]
    where 
        addItem monk item 
            | fst item == mID monk = monk {items = items monk ++ [snd item]}
            | otherwise = monk

sendItems :: [Monkey] -> [(Integer, Integer)] -> [Monkey]
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
    -- print $ solve2 f

