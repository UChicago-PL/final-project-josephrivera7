{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Csv
import System.Environment ( getArgs )
import Data.List ( find, minimumBy )
import Data.Ord ( comparing )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M

-- ===============
-- Data Structures
-- ===============

data Node = Node 
    { name :: String
    , transport_type :: String
    } deriving (Show)

data Bus = Bus
    { bus_number :: String
    , origin :: String
    , destination :: String
    , departure :: Int
    , arrival :: Int
    } deriving (Show)

type Graph = [(Node, [Bus])]

instance FromNamedRecord Bus where
    parseNamedRecord m =
        Bus <$> m.: "bus_number"
            <*> m.: "origin"
            <*> m.: "destination"
            <*> m.: "departure"
            <*> m.: "arrival"

-- ==============================
-- Reading CSV and Creating Graph
-- ==============================

readPeopleCsv :: FilePath -> IO (Either String (Header, V.Vector Bus))
readPeopleCsv filePath = do
    csvData <- BL.readFile filePath
    return $ decodeByName csvData

findEdges :: [Bus] -> String -> [Bus]
findEdges b s = filter (\c -> origin c == s) b

findNodes :: [Bus] -> [String]
findNodes [] = []
findNodes (b:bs) = S.toList(S.fromList (origin b : destination b : findNodes bs))

createNodes :: [String] -> [Node]
createNodes = map (\ t -> Node {name = t, transport_type = "bus"})

createGraph :: [Node] -> [Bus] -> Graph
createGraph ns bs = [(n, findEdges bs (name n)) | n <- ns]

showGraph :: [Bus] -> String
showGraph bs = show $ createGraph ((createNodes . findNodes) bs) bs

-- ================
-- Traversing Graph
-- ================

buildAdj :: Graph -> M.Map String [Bus]
buildAdj graph =
    M.fromList [(name n, bs) | (n, bs) <- graph]

initState :: String -> Int 
    -> ( M.Map String Int, M.Map String Bus, S.Set String, S.Set (Int, String))
initState start startTime = ( M.singleton start startTime, M.empty, S.empty, S.singleton (startTime, start))

dijkstraLoop :: M.Map String [Bus] -> String -> M.Map String Int -> M.Map String Bus
  -> S.Set String -> S.Set (Int, String) -> Maybe (Int, [Bus])
dijkstraLoop adj goal dist parent visited queue
  | S.null queue = Nothing
  | S.member city visited = dijkstraLoop adj goal dist parent visited queue'
  | city == goal = Just (arrivalTime, reconstructPath parent goal)  
  | otherwise = dijkstraLoop adj goal dist' parent' visited' queue''
  where
    ((arrivalTime, city), queue') = S.deleteFindMin queue
    visited' = S.insert city visited
    (dist', parent', queue'') = processCity adj city arrivalTime dist parent queue'

processCity :: M.Map String [Bus] -> String -> Int -> M.Map String Int 
    -> M.Map String Bus -> S.Set (Int, String) -> ( M.Map String Int, M.Map String Bus, S.Set (Int, String))
processCity adj city arrivalTime dist parent queue =
    foldl (relaxBus arrivalTime) (dist, parent, queue) validBuses
  where
    buses = M.findWithDefault [] city adj
    validBuses = filter (\b -> departure b >= arrivalTime) buses

relaxBus :: Int -> ( M.Map String Int, M.Map String Bus, S.Set (Int, String)) -> Bus 
    -> ( M.Map String Int, M.Map String Bus, S.Set (Int, String))
relaxBus currentTime (distAcc, parentAcc, queueAcc) bus
  | maybe True (arrival bus <) (M.lookup (destination bus) distAcc) =
      ( M.insert (destination bus) (arrival bus) distAcc
      , M.insert (destination bus) bus parentAcc
      , S.insert (arrival bus, destination bus) queueAcc
      )
  | otherwise = (distAcc, parentAcc, queueAcc)

reconstructPath :: M.Map String Bus -> String -> [Bus]
reconstructPath parent goal =
    reverse (build goal)
  where
    build city =
        case M.lookup city parent of
            Nothing  -> []
            Just bus -> bus : build (origin bus)

timeAwareDijkstra :: Graph -> String -> String -> Int -> Maybe (Int, [Bus])
timeAwareDijkstra graph start goal startTime =
    dijkstraLoop adj goal dist parent visited queue
  where
    adj = buildAdj graph
    (dist, parent, visited, queue) =
        initState start startTime

showResult :: [Bus] -> String -> String -> Maybe (Int, [Bus])
showResult b origin dest = timeAwareDijkstra graph origin dest 0 where
    nodes = createNodes(findNodes b)
    graph = createGraph nodes b

showPath :: Maybe (Int, [Bus]) -> (Int, [String])
showPath Nothing = (-1, [])
showPath (Just (d, buses)) = (d, [bus_number b | b <- buses])

-- ==============
-- Main Interface
-- ============== 

main :: IO ()
main = do
    args <- getArgs
    let usage = "Incorrect command, please use one from the website"
    case args of 
        ["--one", file, bus_origin, bus_dest] -> do
            result <- readPeopleCsv file
            case result of
                Left err -> putStrLn err
                Right (_, buses) -> print (showPath (showResult (V.toList buses) bus_origin bus_dest))
        _ -> putStrLn usage
                    