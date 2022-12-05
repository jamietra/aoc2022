-- grossly procedural

example :: String
example = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"

stackNumbers :: [[Char]] -> [Int]
stackNumbers ls = map (\s -> read s :: Int) $ words $ head [l | l <- ls, l !! 1 == '1']

parseStackLine :: [Int] -> String -> [Char]
parseStackLine ns l = [l !! ((4 * i) - 3) | i <- ns]

parseStackLines :: [Int] -> [String] -> [[Char]]
parseStackLines ns = map (parseStackLine ns)

getStackLines :: [String] -> [String]
getStackLines = takeWhile (\l -> (l !! 1) /= '1')

buildStacks :: [Int] -> [[Char]] -> [[Char]]
buildStacks ns rows = [dropWhile (== ' ') [s !! (i - 1) | s <- rows] | i <- ns]


getStartStacks :: String -> [[Char]]
getStartStacks input = buildStacks ns $ parseStackLines ns $ getStackLines $ lines input
    where ns = stackNumbers $ lines input

moveOne :: Int -> Int -> [[a]] -> [[a]]
moveOne oldStack newStack stacks = [popPush s | s <- zip [1..] stacks]
    where 
        popPush (i, s)
            | i == oldStack = tail s
            | i == newStack = crate:s
            | otherwise = s
        crate = head (stacks !! (oldStack - 1))


move :: (Int, Int, Int) -> [[a]] -> [[a]]
move (nMoves, oldStack, newStack) = last . take (nMoves + 1) . iterate (moveOne oldStack newStack)

moveMultiple :: (Int, Int, Int) -> [[a]] -> [[a]]
moveMultiple (nmoves, oldStack, newStack) stacks = [popPush s | s <- zip [1..] stacks]
    where 
        popPush (i, s)
            | i == oldStack = drop nmoves s
            | i == newStack = crates ++ s
            | otherwise = s
        crates = take nmoves (stacks !! (oldStack - 1))

parseOneInstruction :: String -> (Int, Int, Int)
parseOneInstruction i = (getNumber w 1, getNumber w 3, getNumber w 5)
    where 
        w = words i
        getNumber word n = read (word !! n)

parseInstructions :: [String] -> [(Int, Int, Int)]
parseInstructions = map parseOneInstruction

getInstructionLines :: String -> [String]
getInstructionLines = tail . dropWhile (/= "") . lines

getInstructions :: String -> [(Int, Int, Int)]
getInstructions = parseInstructions . getInstructionLines

iterateMoves :: [(Int, Int, Int)] -> [[a]] -> [[[a]]]
iterateMoves [] _ = []
iterateMoves (x:xs) stack = newStacks : iterateMoves xs newStacks
    where newStacks = move x stack

getTops :: [[Char]] -> [Char]
getTops = map head

solve1 :: String -> [Char]
solve1 input = getTops $ last $ iterateMoves (getInstructions input) (getStartStacks input)

iterateMoves2 :: [(Int, Int, Int)] -> [[a]] -> [[[a]]]
iterateMoves2 [] _ = []
iterateMoves2 (x:xs) stack = newStacks : iterateMoves2 xs newStacks
    where newStacks = moveMultiple x stack

solve2 :: String -> [Char]
solve2 input = getTops $ last $ iterateMoves2 (getInstructions input) (getStartStacks input)

main :: IO ()
main = do
    f <- readFile "input/input5.txt"
    print $ solve1 f
    print $ solve2 f