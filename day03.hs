import Data.List
import System.Environment ( getArgs )

scores :: [(Int, Char)]
scores = zip [1..] ['a'..'z'] ++ zip [27..] ['A'..'Z']


example :: [String]
example = ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]

split :: String -> (String, String)
split xs = ( , ) (take half xs) (drop half xs)
    where
        half = length xs `div` 2

score :: String -> Int
score xs = sum [fst s | s <- scores, x <- xs, x == snd s]

solveString :: String -> Int
solveString = score . nub . uncurry intersect . split

solve1 :: [String] -> Int
solve1 = sum . map solveString

groupsOfN :: Int -> [a] -> [[a]]
groupsOfN _ [] = []
groupsOfN n xs = take n xs : groupsOfN n (drop n xs)

groupsOfNBetter :: Int -> [a] -> [[a]]
groupsOfNBetter _ [] = []
groupsOfNBetter n xs = start : groupsOfN n rest
    where (start, rest) = splitAt n xs

solve2SingleGroup :: [String] -> Int
solve2SingleGroup = score . nub . foldr intersect ['A'..'z']

solve2 :: [String] -> Int
solve2 = sum . map solve2SingleGroup . groupsOfN 3

main :: IO ()
main = do
    args <- getArgs
    f <- readFile $ head args
    print $ solve1 $ lines f
    print $ solve2 $ lines f
