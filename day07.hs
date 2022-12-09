import Data.Tree
import qualified Data.List as List
import Data.Traversable ( mapAccumL )
import System.Directory.Internal.Prelude (getArgs)

example :: String
example = "$ cd /\n\
\$ ls\n\
\dir a\n\
\14848514 b.txt\n\
\8504156 c.dat\n\
\dir d\n\
\$ cd a\n\
\$ ls\n\
\dir e\n\
\29116 f\n\
\2557 g\n\
\62596 h.lst\n\
\$ cd e\n\
\$ ls\n\
\584 i\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd d\n\
\$ ls\n\
\4060174 j\n\
\8033020 d.log\n\
\5626152 d.ext\n\
\7214296 k\n"

data FileOrDir = File (String, Int) | Dir (String, Int)
    deriving (Show, Eq)

isDir :: FileOrDir -> Bool
isDir (Dir (n, _)) = True
isDir (File _)     = False

getSize :: FileOrDir -> Int
getSize (Dir (_, s)) = s
getSize (File (_, s)) = s

testdir :: Tree FileOrDir
testdir = 
    Node (Dir ("/", 0))
        [
            Node (Dir ("hi", 0)) [Node (File ("foo", 12)) []], 
            Node (File ("boo", 12)) []
        ]

sizeDir :: Tree FileOrDir -> Tree FileOrDir
sizeDir (Node (Dir (n, s)) xs) = 
    Node 
        (Dir 
            (
                n, foldTree sumChildren (Node (Dir (n, s)) xs)
            )
        ) 
        xs
    where
        sumChildren :: FileOrDir -> [Int] -> Int
        sumChildren (Dir (_, s)) xs = s + sum xs
        sumChildren (File (_, s)) xs = s
sizeDir (Node (File c) xs) = Node (File c) xs 

allDirSizes :: Tree FileOrDir -> Tree FileOrDir
allDirSizes (Node c []) = Node c []
allDirSizes (Node c subs) = Node (rootLabel $ sizeDir (Node c subs)) (map allDirSizes subs)

buildFile :: [String] -> [String] -> FileOrDir
buildFile stack [x, y] = File (unwords $ reverse (y:stack), read x)

buildDir :: [String] -> [String] -> FileOrDir
buildDir stack [x, y] = Dir (unwords $ reverse (y:stack), 0)

buildFileOrDir :: String  -> [String] -> FileOrDir
buildFileOrDir xs stack
    | head xs == 'd' = buildDir stack $ words xs
    | otherwise = buildFile stack $ words xs

isCommand :: [Char] -> Bool
isCommand x = head x == '$'

getNextCommandBlock :: [String] -> ([String], [String])
getNextCommandBlock xs = (cleanCommandList $ takeWhile isCommand xs, takeWhile (not . isCommand) $ dropWhile isCommand xs)     

dropNextCommandBlock :: [String] -> [String]
dropNextCommandBlock = dropWhile (not . isCommand) . dropWhile isCommand

getCommandBlocks :: [String] -> [([String], [String])]
getCommandBlocks [] = []
getCommandBlocks xs = getNextCommandBlock xs : getCommandBlocks (dropNextCommandBlock xs)

cleanCommandList :: [String] -> [String]
cleanCommandList = map (drop 3) . filter (/= "ls") . map (drop 2)

buildFileStructure :: [String] -> ([String], [String]) -> ([String], Tree FileOrDir)
buildFileStructure dirStack (commands, results) = 
    (newStack
    , Node (Dir (unwords $ reverse newStack, 0)) (map (\s -> Node (buildFileOrDir s newStack) []) results))
    where 
        newStack = getNewDir dirStack commands

getNewDir :: [String] -> [String] -> [String]
getNewDir = foldl modifyDirStack

modifyDirStack :: [String] -> String -> [String]
modifyDirStack ds d
    | d == ".." = tail ds
    | otherwise = d:ds


mergeTrees :: Tree FileOrDir -> Tree FileOrDir -> Tree FileOrDir
mergeTrees (Node rl sf) t2
    | rl == rootLabel t2 = Node rl [t2]
    | length newTree > length (Node rl sf) = newTree
    | otherwise = Node rl (map (`mergeTrees` t2) sf)
    where 
        newTree = Node rl (attachToSubTrees sf t2)

attachToSubTrees :: [Tree FileOrDir] -> Tree FileOrDir -> [Tree FileOrDir]
attachToSubTrees [] _ = []
attachToSubTrees ((Node rl1 sf1):xs) (Node rl2 sf2) 
    | rl1 == rl2 = Node rl1 sf2 : xs
    | otherwise = Node rl1 sf1 : attachToSubTrees xs (Node rl2 sf2)

actuallyDraw :: Tree FileOrDir -> IO ()
actuallyDraw = putStr . drawTree . fmap show

processBlocks :: [([String], [String])] -> Tree FileOrDir
processBlocks = foldl1 mergeTrees . snd . mapAccumL buildFileStructure []

getSizedDirs :: String -> [FileOrDir]
getSizedDirs = filter isDir . flatten . allDirSizes . processBlocks . getCommandBlocks . lines


solve1 :: String -> Int
solve1 = sum . filter (<= 100000) . map getSize . filter (\(Dir (n, s)) ->  n /= "/") . getSizedDirs

totalSize :: Int
totalSize = 70000000

requiredSize :: Int
requiredSize = 30000000

getNeededToDelete :: [FileOrDir] -> Int
getNeededToDelete xs = requiredSize - (totalSize - List.maximum (map getSize xs)) 

getBiggestLessThan :: Int -> [FileOrDir] -> Int
getBiggestLessThan i xs = head $ dropWhile (<= i) $ List.sort (map getSize xs)

solve2 :: String -> Int
solve2 s = getBiggestLessThan (getNeededToDelete sizeList) sizeList
    where
        sizeList = getSizedDirs s

main :: IO ()
main = do
    args <- getArgs
    f <- readFile $ head args
    print $ solve1 f
    print $ solve2 f
