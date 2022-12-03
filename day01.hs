import Data.List

deLine :: String -> String
deLine "" = "\n"
deLine a  = a ++ " "


parseFile :: String -> [[Int]]
parseFile = map (map (\s -> read s :: Int)) . map words . lines . concat . map deLine . lines

solve1 :: String -> Int
solve1 = maximum . map sum . parseFile

solve2 :: String -> Int
solve2 = sum . take 3 . reverse . sort . map sum . parseFile

main :: IO ()
main = do
    f <- readFile("input1.txt")
    print $ solve1 f
    print $ solve2 f
