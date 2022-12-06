import Data.List

s :: String
s = "bvwbjplbgvbhsrlpgdmjqwftvncz"


solven :: Int -> Int -> String -> Int
solven _ i [] = i
solven n i xs
    | length (nub $ take n xs) == n = (i + n)
    | otherwise = solven n (i + 1) (tail xs)

solve1 :: String -> Int
solve1 = solven 4 0

solve2 :: String -> Int
solve2 = solven 14 0


main :: IO ()
main = do
    f <- readFile "input/input6.txt"
    print $ solve1 f
    print $ solve2 f