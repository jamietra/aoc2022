import Data.List
import System.Directory.Internal.Prelude (getArgs)

example :: String
example = "\
\498,4 -> 498,6 -> 496,6\n\
\503,4 -> 502,4 -> 502,9 -> 494,9\n\
\"

sandStart :: [Int]
sandStart = [500, 0]

type Position = [Int]
type Map = [[Int]]

parseInput :: String -> [[[Int]]]
parseInput = map 
    (map (\x -> read ('[':x ++ "]")::[Int]) . filter (/="->") . words) . nub . lines

handleCollision :: Map -> Position -> (Map, Position)
handleCollision m p@[x, y] 
    | [x, y + 1] == sandStart = (m, [])
    | [x - 1, y + 1] `notElem` m = (m, [x - 1, y + 1])
    | [x + 1, y + 1] `notElem` m = (m, [x + 1, y + 1])
    | otherwise                  = (p:m, p)

getNextSandPosition :: Int -> Map -> Position -> (Map, Position)
getNextSandPosition maxy m p@[x, y] 
    | p `elem` m || y > maxy + 3 = (m, [])
    | [x, y + 1] `notElem` m     = (m, [x, y + 1])
    | [x, y + 1] `elem` m        = handleCollision m p

getNextSandPosition2 :: Int -> Map -> Position -> (Map, Position)
getNextSandPosition2 maxy m p@[x, y] 
    | p `elem` m             = (filter (\[x', y'] -> y' == y || x' /= x) m, [])
    | [x, y + 1] `elem` m    = handleCollision m p
    | y + 1 == maxy + 2      = (p:m, p)
    | [x, y + 1] `notElem` m = (m, [x, y + 1]) 

buildLine :: [Int] -> [Int] -> Map
buildLine [x1, y1] [x2, y2] 
    | x1 == x2 = map (\e -> [x1, e]) [ymin..ymax]
    | y1 == y2 = map (\e -> [e, y1]) [xmin..xmax]
    where
        [ymin, ymax] = sort [y1, y2]
        [xmin, xmax] = sort [x1, x2]

buildSegment :: [Position] -> [Position]
buildSegment xss = concatMap (uncurry buildLine) (zip xss (tail xss))

buildMap :: [[[Int]]] -> Map
buildMap = nub . concatMap buildSegment

getMaxYPos :: Map -> Int
getMaxYPos = maximum . map last

handleOneSand :: (Map -> Position -> (Map, Position)) -> Map -> Map
handleOneSand nextSandGetter m = fst 
    $ last 
    $ takeWhile (not . null . snd) 
    $ iterate (uncurry nextSandGetter) (m, [500 , 0])

-- Shouts out stack overflow
converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

solver :: (Int -> Map -> Position -> (Map, Position)) -> String -> Int
solver nextSandGetter s = length (converge (==) $ iterate (handleOneSand (nextSandGetter maxy)) startMap) - length startMap
    where
        startMap = buildMap $ parseInput s
        maxy = getMaxYPos startMap

solve1 :: String -> Int
solve1 = solver getNextSandPosition

solve2 :: String -> Int
solve2 = solver getNextSandPosition2

main :: IO ()
main = do
    args <- getArgs
    f <- readFile $ head args
    print $ solve1 f
    print $ solve2 f