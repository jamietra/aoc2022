import Data.Char
import Data.List (takeWhile, sort)

newtype Packet = Packet String deriving (Show)

instance Eq Packet where
    (==) (Packet x) (Packet y) = x == y

instance Ord Packet where
    (>) (Packet x) (Packet y) = not $ inOrder [x, y]
    (<) (Packet x) (Packet y) = not $ inOrder [y, x]
    (<=) px@(Packet x) py@(Packet y) = px < py || px == py
    (>=) px@(Packet x) py@(Packet y) = px > py || px == py

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split c xs = fst spanned:split c (snd spanned)
  where
    noC = dropWhile (== c) xs
    spanned = span (/= c) noC

handleBothDigits :: String -> String -> Bool
handleBothDigits xs ys = 
    if numx == numy 
        then inOrder [restx, resty]
        else (read numx::Int) < (read numy::Int)
    where
        [(numx, restx), (numy, resty)] = map (span isDigit) [xs, ys]

skipBoth :: Char -> Char -> Bool
skipBoth x y = x `elem` skipChars && x == y
    where skipChars = ",[]"

putInList :: String -> String
putInList xs = '[' : numx ++ ']' : restx
    where (numx, restx) = span isDigit xs

inOrder :: [String] -> Bool
inOrder [x:xs, y:ys]
  | skipBoth x y   = inOrder [xs, ys]
  | isDigit x && isDigit y = handleBothDigits (x:xs) (y:ys)
  | x == '[' && isDigit y  = inOrder [x:xs, putInList (y:ys)]
  | isDigit x && y == '['  = inOrder [putInList (x:xs), y:ys]
  | x == ']'               = True
  | y == ']'               = False
  -- no otherwise so we get an error if we missed something

buildList :: String -> [Int]
buildList = undefined

solve1 :: String -> Int
solve1 = sum . map fst . filter snd . zip [1..] . map inOrder . split "" . lines 

solve2 :: String -> Int
solve2 s = product $ map fst $ filter (\(_, Packet x) -> x `elem` ["[[2]]", "[[6]]"]) $ zip [1..] $ sort packets
    where
        packets = [Packet "[[2]]", Packet "[[6]]"] ++ map Packet (filter (/= "") $ lines s)

main :: IO ()
main = do
    f <- readFile "input/input13.txt"
    print $ solve1 f
    print $ solve2 f
