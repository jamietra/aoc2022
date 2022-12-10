import Data.List

type Instruction = (String, Int -> Int)

readInstruction :: [String] -> Instruction
readInstruction [x] = (x, id)
readInstruction ["addx", y] = ("addx", (+) yint)
    where
        yint = read y::Int

parseInput :: String -> [Instruction]
parseInput = map (readInstruction . words) . lines

instructionToRepeats :: Instruction -> Int
instructionToRepeats ("noop", _)  = 1
instructionToRepeats ("addx", _)  = 2

addInstruction :: [Int] -> Instruction ->  [Int]
addInstruction [idx, rx] i = [idx + instructionToRepeats i, snd i rx]

instructionsToRegisterSeries :: [Instruction] -> [[Int]]
instructionsToRegisterSeries = scanl addInstruction [1, 1]

getSumProds :: [Int] -> [[Int]] -> Int
getSumProds idxs regs = sum [i * getSprite i regs | i <- idxs]

getSprite :: Int -> [[Int]] -> Int
getSprite i regs = last (takeWhile (\[x, y] -> x <= i) regs) !! 1

solve1 :: String -> Int
solve1 = getSumProds (take 6 (20:[20 + i * 40 | i <- [1..]])) . instructionsToRegisterSeries . parseInput

getCharToDraw :: [[Int]] -> Int -> Char
getCharToDraw sprites clock = if pos `elem` [s-1..s+1] then '#' else '.'
    where
        pos = (clock - 1) `mod` 40
        s = getSprite clock sprites

clockGroups :: [[Int]]
clockGroups = [[40 * i + 1.. 40*i + 40]| i <- [0..5]]

solve2 :: String -> [String]
solve2 input = (map . map) (getCharToDraw sprites) clockGroups
    where sprites = instructionsToRegisterSeries $ parseInput input

main :: IO ()
main = do
    f <- readFile "input/input10.txt"
    print $ solve1 f
    putStr $ unlines $ solve2 f