import Algorithm.Search
import Data.List
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)

type State = (Int, Int, Char)
type ElevMap = [[Char]]

findHeights :: ElevMap -> Char -> [State]
findHeights m h = [(i, j, newElev) | i <- [0..length m - 1], j <- [0..length (head m) - 1], (m !! i) !! j == h]
    where 
        newElev = if h == 'S' then 'a' else h

getNewState :: ElevMap -> State -> State
getNewState m (r, c, _) = (r, c, (m !! clippedRow) !! clippedCol)
    where
        clippedRow = max (min r (length m - 1)) 0
        clippedCol = max ( min c (length (head m) - 1)) 0

combineCoords :: ElevMap -> State -> [[Int]] -> [State]
combineCoords m (r, c, h) [rs, cs] = [getNewState m (r', c, h) | r' <- rs] ++ [getNewState m (r, c', h) | c' <- cs]

neighbours :: ElevMap -> State -> [State]
neighbours m s@(r, c, h) = filter (isValidNext m s) $ combineCoords m s $ map (zipWith (+) [-1, 1]) [repeat r, repeat c]

isValidNext :: ElevMap -> State -> State -> Bool
isValidNext m (r, c, h) (r', c', h') = r' >= 0 && c' >=0 && fromEnum replacedH - fromEnum  h <= 1 && r' < length m && c' < length (head m)
    where
        replacedH = if h' == 'E' then 'z' else h'

distance :: State -> State -> Int
distance (r, c, _) (r', c', _) = abs (c -c') + abs (r - r') 

isEnd :: State -> Bool
isEnd (_, _, h) = h == 'E'

readInput :: String -> [[Char]]
readInput = lines


solve1 :: String -> Int
solve1 s = fst $ fromJust $ dijkstra (neighbours m) distance isEnd start
    where
        m = readInput s
        start = head $ findHeights m 'S'

extractBestDistance :: [Maybe (Int, [State])] -> [Int]
extractBestDistance results = sort [fst $ fromJust r | r <- results, isJust r]

solve2 :: String -> Int
solve2 s = head $ extractBestDistance $ map (dijkstra (neighbours m) distance isEnd) starts
    where
        starts = findHeights m 'S' ++ findHeights m 'a'
        m = readInput s

main :: IO ()
main = do
    args <- getArgs
    f <- readFile $ head args
    print $ solve1 f
    print $ solve2 f
