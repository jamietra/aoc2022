import Data.List
import System.Directory.Internal.Prelude (getArgs)

type Pos = [Int]
type Instruction = Char 

dToPosDelta :: Char -> [Int]
dToPosDelta 'L' = [-1, 0 ]
dToPosDelta 'R' = [ 1, 0 ]
dToPosDelta 'U' = [ 0, 1 ]
dToPosDelta 'D' = [0, -1 ]

addPos :: Pos -> Pos -> Pos
addPos = zipWith (+)

diff :: Pos -> Pos -> Pos
diff = zipWith (-)

moveHead :: Instruction -> Pos -> Pos
moveHead d h = addPos h (dToPosDelta d)

moveTail :: Pos -> Pos -> Pos
moveTail headPos tailPos
    | maximum (map abs $ diff headPos tailPos) <= 1 = tailPos
    | otherwise = catchTailUp tailPos headPos

catchTailUp :: Pos -> Pos -> Pos
catchTailUp [hx, hy] [tx, ty] = [tx + dx - signum dx, ty + dy - signum dy]
    where
        dx = hx - tx
        dy = hy - ty

moveAll :: [Pos] -> Instruction -> [Pos]
moveAll (p:ps) i = scanl1 moveTail (moveHead i p : ps)

parseInput :: String -> String
parseInput = concatMap ((\[d, n] -> replicate (read n::Int) (head d)) . words) . lines

solver :: [Pos] -> String -> Int
solver ps s = length $ nub $ map last $ scanl moveAll ps $ parseInput s

solve1 :: String -> Int
solve1 = solver [[0, 0], [0, 0]]

solve2 :: String -> Int
solve2 = solver (replicate 10 [0, 0])

main :: IO ()
main = do
    args <- getArgs
    f <- readFile $ head args
    print $ solve1 f
    print $ solve2 f