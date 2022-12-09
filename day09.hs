import Data.List

example :: String
example = "\
\R 4\n\
\U 4\n\
\L 3\n\
\D 1\n\
\R 4\n\
\D 1\n\
\L 5\n\
\R 2"

example2 :: String
example2 = "\
\R 5\n\
\U 8\n\
\L 8\n\
\D 3\n\
\R 17\n\
\D 10\n\
\L 25\n\
\U 20"

type Pos = [Int]
type Instruction = Char 

plusOrMinus :: Char -> Int
plusOrMinus c 
    | c `elem` "LD" = -1
    | otherwise = 1

addPos :: Pos -> Pos -> Pos
addPos = zipWith (+)

diff :: Pos -> Pos -> Pos
diff = zipWith (-)

moveHead :: Instruction -> Pos -> Pos
moveHead d h
    | d `elem` "LR" = addPos h [p, 0]
    | d `elem` "UD" = addPos h [0, p]
    | otherwise = error "What is this direction"
    where
        p = plusOrMinus d

moveTail :: Pos -> Pos -> Pos
moveTail tailPos headPos
    | maximum (map abs $ diff headPos tailPos) <= 1 = tailPos
    | otherwise = catchTailUp tailPos headPos

catchTailUp :: Pos -> Pos -> Pos
catchTailUp [hx, hy] [tx, ty] = [tx + dx - signum dx, ty + dy - signum dy]
    where
        dx = hx - tx
        dy = hy - ty

moveRest :: [Pos] -> [Pos]
moveRest [p]   = [p]
moveRest (p1:p2:ps) = p1 : moveRest (newp2:ps)
    where 
        newp2 = moveTail p2 p1

moveAll :: [Pos] -> Instruction -> [Pos]
moveAll (p:ps) i = moveRest (moveHead i p : ps)

parseInput :: String -> String
parseInput = concatMap ((\[d, n] -> replicate (read n::Int) (head d)) . words) . lines


solve1 :: String -> Int
solve1 = length . nub . map last . scanl moveAll [[0, 0], [0, 0]] . parseInput

solve2 :: String -> Int
solve2 = length . nub . map last . scanl moveAll (replicate 10 [0, 0]) . parseInput

main :: IO ()
main = do
    f <- readFile "input/input9.txt"
    print $ solve1 f
    print $ solve2 f