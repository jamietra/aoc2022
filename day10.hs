import Data.List
import Data.Sequence (chunksOf)

type Instruction = (String, Int -> Int)

readInstruction :: [String] -> Instruction
readInstruction [x] = (x, id)
readInstruction ["addx", y] = ("addx", (+) yint)
    where
        yint = read y::Int

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

parseInput :: String -> [Instruction]
parseInput = map (readInstruction . words) . lines

instructionToRepeats :: Instruction -> Int
instructionToRepeats ("noop", _)  = 1
instructionToRepeats ("addx", _)  = 2

addInstruction :: [Int] -> Instruction ->  [Int]
addInstruction (x:xs) i = snd i x : replicate (instructionToRepeats i) x ++ xs 

instructionsToRegisterSeries :: [Instruction] -> [Int]
instructionsToRegisterSeries = reverse . foldl' addInstruction [1]

getSumProds :: [Int] -> [Int] -> Int
getSumProds idxs regs = sum $ zipWith (*) (map (\i -> regs !! (i - 1)) idxs) idxs

solve1 :: String -> Int
solve1 = getSumProds (take 6 (20:[20 + i * 40 | i <- [1..]])) . instructionsToRegisterSeries . parseInput

getCharToDraw :: [Int] -> Int -> Char
getCharToDraw sprites clock = if abs (pos - s) <= 1 then '⬜' else '⬛'
    where
        pos = (clock - 1) `mod` 40
        s = sprites !! (clock - 1)

solve2 :: String -> [String]
solve2 input = splitEvery 40 $ map (getCharToDraw sprites) [1..240]
    where sprites = instructionsToRegisterSeries $ parseInput input

main :: IO ()
main = do
    f <- readFile "input/input10.txt"
    print $ solve1 f
    putStr $ unlines $ solve2 f