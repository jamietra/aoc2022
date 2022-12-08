import Data.List

s :: String
s = "bvwbjplbgvbhsrlpgdmjqwftvncz"


solven :: Int -> Int -> String -> Int
solven _ i [] = i
solven n i (x:xs)
    | length (nub $ take n (x:xs)) == n = i + n
    | otherwise = solven n (i + 1) xs

solve1 :: String -> Int
solve1 = solven 4 0

solve2 :: String -> Int
solve2 = solven 14 0


main :: IO ()
main = do
    f <- readFile "input/input6.txt"
    print $ length f
    print $ solve1 $ concat $ replicate 100 f
    print $ solve2 $ concat $ replicate 100 f