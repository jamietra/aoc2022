import Data.List
import Data.IntMap.Merge.Lazy (merge)
import Language.Haskell.TH (fromE)
import System.Directory.Internal.Prelude (getArgs)


example = "\
    \30373\n\
    \25512\n\
    \65332\n\
    \33549\n\
    \35390"

parseInput :: String -> [[Int]]
parseInput = map stringToInts . lines

stringToInts :: String -> [Int]
stringToInts = map (read . (:""))

runningMax :: Ord a => [a] -> [a]
-- offset so it's largest seen before this element
runningMax (x:xs) = x : scanl1 max (x:xs)

interiorVisible :: [Int] -> [Bool]
interiorVisible xs = init $ tail $ zipWith (<) (runningMax xs) xs

bothDirections :: [Int] -> [Bool]
bothDirections xs = zipWith (||) (interiorVisible xs) (reverse $ interiorVisible (reverse xs))

mapInteriorVisible :: [[Int]] -> [[Bool]]
mapInteriorVisible = map bothDirections . init . tail

mergeGridsWith :: (a -> a -> b) -> [[a]] -> [[a]] -> [[b]]
mergeGridsWith f xs ys = [zipWith f (xs !! i) (ys !! i) | i <- [0..length xs - 1]]

solve1 :: String -> Int
solve1 s = sum (
    map (sum . map fromEnum) 
        $ mergeGridsWith (||) 
            (mapInteriorVisible xss) 
            (transpose $ mapInteriorVisible $ transpose xss)
    ) 
    + (4 * length xss - 4)

    where xss = parseInput s

rowViewDistances :: [Int] -> [Int]
rowViewDistances [] = []
rowViewDistances (x:xs) = length (takeWhile (< x) xs) + notEnd : rowViewDistances xs
    where notEnd = fromEnum $ not . null $ dropWhile (< x) xs

bothDistances :: [Int] -> [Int]
bothDistances xs = zipWith (*) (rowViewDistances xs) (reverse $ rowViewDistances $ reverse xs)

solve2 :: String -> Int
solve2 s = maximum $ map maximum $ mergeGridsWith (*) (map bothDistances xss) (transpose $ map bothDistances $ transpose xss)
    where
        xss = parseInput s

visualize :: Enum a => [[a]] -> IO ()
visualize xss = putStr $ unlines $ map (unwords . map ((\s -> if s == "1" then "1" else " ") . show . fromEnum)) xss

main :: IO ()
main = do
    args <- getArgs
    f <- readFile $ head args
    -- visualize $ mergeGridsWith (||) (mapInteriorVisible (parseInput f)) (transpose $ mapInteriorVisible $ transpose (parseInput f))
    print $ solve1 f
    print $ solve2 f