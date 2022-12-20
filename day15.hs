import Data.List
import Data.Char
import qualified Data.IntSet as Set


type Sensor = ((Int, Int), Int)

parseLine :: String -> [[Int]]
parseLine s = [[sx, sy], [bx, by]]
    where 
        [sx, sy, bx, by]  = map ((\cs -> read cs :: Int) . filter (\c -> c == '-' || isNumber c)) 
            $ filter (\s -> head s `elem` "xy") 
            $ words s

parseInput :: String -> [[[Int]]]
parseInput = map parseLine . lines

createBeacon ::  [[Int]] -> [Sensor] -> [Sensor]
createBeacon [[sx, sy], [bx, by]] m = ((sx, sy), distance):m
    where
        distance = abs (sx - bx) + abs (sy - by)

buildMap :: [[[Int]]] -> [Sensor]
buildMap = foldr createBeacon []

sensorsWithY :: [Sensor] -> Int -> [Sensor]
sensorsWithY ss y = filter containY ss
    where
        containY ((_, sy), d) = y >= sy - d && y <= sy + d 

getXBounds :: Int -> Sensor -> (Int, Int)
getXBounds y ((sx, sy), d) = (sx - dx, sx + dx)
    where 
        dx = d - abs (y - sy)

getXRange :: [Sensor] -> Int -> [Int]
getXRange ss y = [fst $ head $ sortOn fst bs..snd $ last $ sortOn snd bs]
    where 
        bs = map (getXBounds y) $ sensorsWithY ss y

solver1 :: Int -> String ->  Int
solver1 y s = Set.size (intervalToSet $ map (getXBounds y) sensors) - 1
    where
        sensors = flip sensorsWithY y $ buildMap $ parseInput s

solve1 :: String -> Int
solve1 = solver1 2000000

mergeIntervals :: [(Int, Int)] -> [(Int,Int)]
-- try and combine intervals sorted by left bound into a single one
mergeIntervals [(a, b)] = [(a, b)]
mergeIntervals is@((a1, b1):(a2, b2):rest) 
    | a2 <= b1 = mergeIntervals ((a1, max b1 b2):rest)
    | otherwise = is

intervalToSet :: [(Int, Int)] -> Set.IntSet
intervalToSet = Set.unions . map (\ (a,b) -> Set.fromAscList [a..b])

checkRow :: [Sensor] -> Int -> Set.IntSet  -> [Int]
checkRow m y xSet =  
    if length intervals == 1
        then []
        else
            Set.toList 
            $ xSet Set.\\ Set.unions 
                (map (\ (a, b) -> Set.intersection (Set.fromAscList [a .. b]) xSet) intervals) 
    where
        intervals =  mergeIntervals $ sortOn fst $ map (getXBounds y) $ sensorsWithY m y
    

solve2 :: String -> Int
solve2 s = 4_000_000 * head xs + y 
    where 
        xmax = 4_000_000
        (y, xs) = head $ dropWhile (null . snd) [(y, checkRow sMap y xSet) | y <- ySearchSpace]
        xSearchSpace = [0..xmax]
        xSet = Set.fromAscList xSearchSpace
        ySearchSpace = [0..xmax]
        sMap = buildMap $ parseInput s

main :: IO ()
main = do
    f <- readFile "input/input15.txt"
    print $ solve1 f
    print $ solve2 f
