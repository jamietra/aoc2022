import Data.Char (isDigit)
import Data.Map.Strict ((!))
import Data.List
import qualified Data.Map.Strict as Map

example = "\
\Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
\Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
\Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
\Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
\Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
\Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
\Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
\Valve HH has flow rate=22; tunnel leads to valve GG\n\
\Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
\Valve JJ has flow rate=21; tunnel leads to valve II\n\
\"

data Valve = Valve {
      name :: String
    , rate :: Int
    , neighbours :: [String]
} deriving (Show, Eq, Ord)

type ValveMap = Map.Map String Valve

data State = State {
      remainingTime :: Int
    , position :: String
    , openValves :: [String]
    , nOpenableValves :: Int
    , vMap :: ValveMap
    , allOpen :: Bool
} deriving (Show, Eq, Ord)

getValve :: State -> Valve
getValve s = vMap s ! position s

currentValveOpenable :: State -> Bool
currentValveOpenable s = position s `notElem` openValves s && rate (getValve s) /= 0

data Action = Open Bool | Move String
    deriving (Eq, Show, Ord)

parseLine :: String -> Valve
parseLine l = Valve {
      name = vname
    , rate = vrate
    , neighbours = vneighbours
    }
    where
        ws = words $ filter (`notElem` ",;") l
        vname = ws !! 1
        vrate = read (dropWhile (not . isDigit) $ ws !! 4) :: Int
        vneighbours = drop 9 ws

parseInput :: String -> [Valve]
parseInput = map parseLine . lines

buildValveMap :: [Valve] -> ValveMap
buildValveMap = Map.fromList . map (\v -> (name v, v))

getActions :: State -> [Action]
getActions s = filter (/= Open False) $ Open (currentValveOpenable s): map Move (neighbours $ getValve s)

updateState :: State -> Action -> State
updateState s@State{remainingTime = rt, openValves=openV, allOpen=a, position = p, nOpenableValves=n} (Open _) =
        s {
        remainingTime = rt - 1
        , openValves = newOpened
        , allOpen = length newOpened == n
    }
    where 
        newOpened = sort $ p:openV
updateState s@State{remainingTime = rt} (Move newP) = s {remainingTime = rt - 1, position = newP}

valueFunction :: (State -> Action -> Int) -> State -> Action -> Int
valueFunction _ State{allOpen = True} _ = 0
valueFunction _ State{remainingTime = 0} _ = 0
valueFunction mvf s a@(Open _) = newAmt + maximum (map (mvf newState) newActions)
    where
        newState = updateState s a
        newAmt = rate (getValve s) * (remainingTime s - 1)
        newActions = getActions newState
valueFunction mvf s a@(Move newP) = maximum $ map (mvf newState) newActions
    where
        newState = updateState s a
        newActions = getActions newState

valueMap :: Map.Map (State, Action) Int
valueMap = Map.fromList [(sa, uncurry memoValue sa) | sa <- allStateActions]

memoValue :: State -> Action -> Int
memoValue s a = valueMap ! (s,a)

allStateActions :: [(State, Action)]
allStateActions = undefined

solve1 :: String -> Int
solve1 s = maximum $ map (memoValue startState) startActions
    where
        (startState, startActions) = getStartState s

getStartState :: String -> (State, [Action])
getStartState s = (startState, startActions)
    where
        valves = parseInput s
        m = buildValveMap valves
        openable = length [v | v <- valves, rate v /= 0]
        startState = State {
              position = "AA"
            , vMap = m
            , openValves = []
            , remainingTime = 30
            , nOpenableValves = openable
            , allOpen = False
        }
        startActions = getActions startState
        
