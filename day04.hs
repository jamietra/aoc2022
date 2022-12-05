import Data.List
import Data.Function

contained :: [[Int]] -> Bool
contained [xs, ys] = xInty == smallerL
    where 
        xInty = xs `intersect` ys
        smallerL = minimumBy (compare `on` length) [xs, ys]

anyOverlap :: [[Int]] -> Bool
anyOverlap [xs, ys] = not (null (xs `intersect` ys))

replace :: String -> String
replace cs = [repl c | c <- cs]
    where 
        repl :: Char -> Char
        repl ',' = '\n'
        repl '-' = ' '
        repl ch = ch
          

stringToRange :: String -> (Int, Int)
stringToRange = undefined

parseLine :: String -> [[Int]]
parseLine = map ((\[x, y] -> [x..y]) . map (\s -> read s::Int) . words) . lines . replace

parseInput :: String -> [[[Int]]]
parseInput = map parseLine . lines


solver :: ([[Int]] -> Bool) -> String -> Int
solver solveFunction s = sum . map (fromEnum . solveFunction) $ parseInput s

solve1 :: String -> Int
solve1 = solver contained

solve2 :: String -> Int
solve2 = solver anyOverlap

main :: IO ()
main = do
    f <- readFile "input/input4.txt"
    print $ solve1 f
    print $ solve2 f

