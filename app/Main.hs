{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Csv
import System.IO
import System.Environment ( getArgs )
import Data.List ( minimumBy )
import Data.Ord ( comparing )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M

-- ===============
-- Data Structures
-- ===============

newtype Node = Node 
{-  Create a new type to hold the name of cities/stops. -}
    { name :: String } 
    deriving (Show)

data Bus = Bus
{-  Create a data type to hold information about the graphs edges 
    Departure and arrival time held in range 000-2359. -}
    { bus_number :: String
    , origin :: String
    , destination :: String
    , departure :: Int
    , arrival :: Int
    , price :: Double
    } deriving (Show)

type Graph = [(Node, [Bus])]
{-  A graph is a list of tuples, the first element is a node (city), second is 
    list of edges (buses). -}

instance FromNamedRecord Bus where
{-  Parses the information from CSV into record. -}
    parseNamedRecord m =
        Bus <$> m.: "bus_number"
            <*> m.: "origin"
            <*> m.: "destination"
            <*> m.: "departure"
            <*> m.: "arrival"
            <*> m.: "price"

-- ==============================
-- Reading CSV and Creating Graph
-- ==============================

readTransportCsv :: FilePath -> IO (Either String (Header, V.Vector Bus))
{-  Reads the CSV file into the data records. -}
readTransportCsv filePath = do
    csvData <- BL.readFile filePath
    return $ decodeByName csvData

findEdges :: [Bus] -> String -> [Bus]
{-  Filter list of buses to see if their origin matches the passed in string. -}
findEdges b s = filter (\c -> origin c == s) b

findNodes :: [Bus] -> [String]
{-  Create set from origin and departure strings and then convert back to list. -}
findNodes [] = []
findNodes (b:bs) = S.toList(S.fromList (origin b : destination b : findNodes bs))

createNodes :: [String] -> [Node]
{-  Create list of nodes -}
createNodes = map (\ t -> Node {name = t})

createGraph :: [Node] -> [Bus] -> Graph
{-  Create a graph from the passed in Nodes and Buses. -}
createGraph ns bs = [(n, findEdges bs (name n)) | n <- ns]

-- ==========================================
-- Traversing Graph via Quickest Arrival Time
-- ==========================================

buildAdj :: Graph -> M.Map String [Bus]
{-  Build adjacency list between strings and lists of buses. -}
buildAdj graph =
    M.fromList [(name n, bs) | (n, bs) <- graph]

initState :: String -> Int 
    -> ( M.Map String Int, M.Map String Bus, S.Set String, S.Set (Int, String))
{-  Create a mutable object for tracking the state of the algorithm.  
    Take in a starting location and a starting time. 
    Return Map for start and startTime, empty Map for parent nodes, 
    empty Set for visited Nodes, Set for traversal queue. -}
initState start startTime = ( M.singleton start startTime, M.empty, S.empty, S.singleton (startTime, start))

dijkstraLoop :: M.Map String [Bus] -> String -> M.Map String Int -> M.Map String Bus
  -> S.Set String -> S.Set (Int, String) -> Maybe (Int, [Bus])
{-  Perform the loop for the algorithm. 
    If the queue is empty, return Nothing.
    If the city has been visited, redo the loop, only changing the queue.
    If the city is the destination, return the state elements. 
    Otherwise, redo the loop with updated distance, parents, and queue. -}
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
{-  Relaxes the buses if they occur at or after the arrival time and then updates the state -}
processCity adj city arrivalTime dist parent queue =
    foldl (relaxBusTime arrivalTime) (dist, parent, queue) validBuses
  where
    buses = M.findWithDefault [] city adj
    validBuses = filter (\b -> departure b >= arrivalTime) buses

relaxBusTime :: Int -> ( M.Map String Int, M.Map String Bus, S.Set (Int, String)) -> Bus 
    -> ( M.Map String Int, M.Map String Bus, S.Set (Int, String))
{-  If the next bus is valid, update the state and return it
    Otherwise, return the original state. -}
relaxBusTime currentTime (distAcc, parentAcc, queueAcc) bus
  | maybe True (arrival bus <) (M.lookup (destination bus) distAcc) =
      ( M.insert (destination bus) (arrival bus) distAcc
      , M.insert (destination bus) bus parentAcc
      , S.insert (arrival bus, destination bus) queueAcc
      )
  | otherwise = (distAcc, parentAcc, queueAcc)

reconstructPath :: M.Map String Bus -> String -> [Bus]
{-  Reconstruct the path from the destination to the origin and then reverse it. -}
reconstructPath parent goal =
    reverse (build goal)
  where
    build city =
        case M.lookup city parent of
            Nothing  -> []
            Just bus -> bus : build (origin bus)

timeAwareDijkstra :: Graph -> String -> String -> Int -> Maybe (Int, [Bus])
{-  Call the the algorithm loop and output the result. -}
timeAwareDijkstra graph start goal startTime =
    dijkstraLoop adj goal dist parent visited queue
  where
    adj = buildAdj graph
    (dist, parent, visited, queue) =
        initState start startTime

-- ========================================
-- Traversing Graph via Cheapest Total Cost
-- ========================================

initStateCost :: String -> Int
    -> ( M.Map String Double, M.Map String Bus, S.Set String, S.Set (Double, Int, String))
{-  Create a mutable object for tracking the state of the algorithm, similar to as initState.
    This time, the first Map and last Set have fields for price which is stored as
    a Double. -}
initStateCost start startTime = ( M.singleton start 0.0, M.empty, S.empty, S.singleton (0.0, startTime, start))

dijkstraLoopCost :: M.Map String [Bus] -> String -> M.Map String Double -> M.Map String Bus
  -> S.Set String -> S.Set (Double, Int, String) -> Maybe (Double, [Bus])
{-  Perform the loop for the algorithm, operates similar to dijkstraLoop.
    Difference is cost is passed through the algo so Double field is added. -}
dijkstraLoopCost adj goal dist parent visited queue
  | S.null queue = Nothing
  | S.member city visited = dijkstraLoopCost adj goal dist parent visited queue'
  | city == goal = Just (cost, reconstructPath parent goal)  
  | otherwise = dijkstraLoopCost adj goal dist' parent' visited' queue''
  where
    ((cost, arrivalTime, city), queue') = S.deleteFindMin queue
    visited' = S.insert city visited
    (dist', parent', queue'') = processCityCost adj city cost arrivalTime dist parent queue'

processCityCost :: M.Map String [Bus] -> String -> Double -> Int -> M.Map String Double
  -> M.Map String Bus -> S.Set (Double, Int, String) 
  -> ( M.Map String Double, M.Map String Bus, S.Set (Double, Int, String))
{-  Similar to processCity but includes cost field -}
processCityCost adj city cost arrivalTime dist parent queue =
    foldl (relaxBusCost cost arrivalTime) (dist, parent, queue) validBuses
  where
    buses = M.findWithDefault [] city adj
    validBuses = filter (\b -> departure b >= arrivalTime) buses

relaxBusCost :: Double -> Int -> ( M.Map String Double, M.Map String Bus, S.Set (Double, Int, String))
  -> Bus -> ( M.Map String Double, M.Map String Bus, S.Set (Double, Int, String))
{-  If the next bus is valid, update the state and return it
    Otherwise, return the original state. -}
relaxBusCost currentCost currentTime (distAcc, parentAcc, queueAcc) bus
  | maybe True (newCost <) (M.lookup dest distAcc) =
      ( M.insert dest newCost distAcc
      , M.insert dest bus parentAcc
      , S.insert (newCost, arrival bus, dest) queueAcc
      )
  | otherwise = (distAcc, parentAcc, queueAcc)
  where
    dest = destination bus
    newCost = currentCost + price bus

reconstructCostPath :: M.Map String Bus -> String -> [Bus]
{-  Reconstruct the path from the destination to the origin and then reverse it. -}
reconstructCostPath parent goal =
    reverse (build goal)
  where
    build city =
        case M.lookup city parent of
            Nothing  -> []
            Just bus -> bus : build (origin bus)

costAwareDijkstra :: Graph -> String -> String -> Int -> Maybe (Double, [Bus])
{-  Call the the cost algorithm loop and output the result. -}
costAwareDijkstra graph start goal startTime =
    dijkstraLoopCost adj goal dist parent visited queue
  where
    adj = buildAdj graph
    (dist, parent, visited, queue) =
        initStateCost start startTime

-- ===============
-- Display Results
-- ===============

showResult :: [Bus] -> String -> String -> Maybe (Int, [Bus])
{-  Create graph from list of buses, origin string, and destination string.
    Calls the algorithm and outputs the result. -}
showResult b origin dest = timeAwareDijkstra graph origin dest 0 where
    nodes = createNodes(findNodes b)
    graph = createGraph nodes b

showPath :: Maybe (Int, [Bus]) -> (Int, [String])
{-  If no path exists, output a negative time value and an empty list.
    Else, output the end time and formatted list of output strings. -}
showPath Nothing = (-1, ["A valid path could not be found."])
showPath (Just (d, buses)) = (d, pathStr buses ++ totalStr (d, buses))

pathStr :: [Bus] -> [String]
{-  Create output strings from passed in list of buses. -}
pathStr = map (\b -> "Take " ++ bus_number b ++ " from " ++ origin b ++ " to " 
    ++ destination b ++ " (" ++ show (departure b) ++ "-" ++ show (arrival b)
    ++ ")" ++ " [" ++ show (price b) ++ "]")

totalStr :: (Int, [Bus]) -> [String]
{-  Create a list with a single string containing the final information. -}
totalStr (d, buses) = ["\nTotal time to get to " ++ (destination . last) buses 
    ++ ": " ++ show diff ++ " hours"]
    where
        start = (departure . head) buses
        diff = fromIntegral (d - start) / 100.0

comparePaths :: (Int, [String]) -> (Int, [String]) -> (Int, [String])
{-  Compares two paths and returns the one with the earlier time.
    Filters out any invalid paths that have a negative time. -}
comparePaths op1 op2 = minimumBy (comparing fst) (filter (\(x, _) -> x > 0 ) [op1, op2])

showCostResult :: [Bus] -> String -> String -> Maybe (Double, [Bus])
{-  Create graph from list of buses, origin string, and destination string.
    Calls the cost algorithm and outputs the result. -}
showCostResult b origin dest = costAwareDijkstra graph origin dest 0 where
    nodes = createNodes(findNodes b)
    graph = createGraph nodes b
    
showCostPath :: Maybe (Double, [Bus]) -> (Double, [String])
{-  If no path exists, output a negative time value and an empty list.
    Else, output the end time and formatted list of output strings. -}
showCostPath Nothing = (-1, ["A valid path could not be found."])
showCostPath (Just (d, buses)) = (d, pathStr buses ++ costTotalStr (d, buses))

costTotalStr :: (Double, [Bus]) -> [String]
{-  Create a list with a single string containing the final information. -}
costTotalStr (d, buses) = ["\nTotal cost to get to " ++ (destination . last) buses 
    ++ ": $" ++ show d]

compareCostPaths :: (Double, [String]) -> (Double, [String]) -> (Double, [String])
{-  Compares two paths and returns the one with the lower cost.
    Filters out any invalid paths that have a negative time. -}
compareCostPaths op1 op2 = minimumBy (comparing fst) (filter (\(x, _) -> x > 0 ) [op1, op2])

-- ==============
-- Main Interface
-- ============== 

prompt :: String -> IO String
{-  Prompt function taken from Lecture 12 (Intermediate I/O Lesson) -}
prompt msg = putStr msg *> hFlush stdout *> getLine

looping :: String -> [String] -> IO String
{-  Continually prompt user for a string that matches one of the approved
    options. -}
looping p options = do
    str <- prompt p
    check str
  where
    check s
        | s `elem` options = return s
        | otherwise = looping p options

main :: IO ()
main = do 
    requirement <- looping "What do you care about most? (time or price)\n" ["time", "price"]
    origin_str <- prompt "Where are you starting from?\n"
    dest_str <- prompt "Where is your destination?\n"
    nstr <- looping "How many modes of transport do you want to use? (one or two)\n" ["one", "two"]
    case nstr of
        "one" -> do
            file1 <- prompt "Path of file:\n"
            res1 <- readTransportCsv file1
            case res1 of
                Left err -> putStrLn err
                Right (_, buses) -> do
                    case requirement of 
                        "time" -> putStrLn (unlines $ snd (showPath (showResult (V.toList buses) origin_str dest_str)))
                        "price" -> putStrLn (unlines $ snd (showCostPath (showCostResult (V.toList buses) origin_str dest_str)))
        "two" -> do
            cstr <- looping "Do you want to use separate paths or combine them? (separate or combined)\n" ["separate", "combined"]
            case cstr of
                "separate" -> do
                    file1 <- prompt "Path of file 1:\n"
                    res1 <- readTransportCsv file1
                    case res1 of
                        Left err -> putStrLn err
                        Right (_, buses) -> do
                            file2 <- prompt "Path of file 2:\n"
                            res2 <- readTransportCsv file2
                            case res2 of
                                Left err -> putStrLn err
                                Right (_, trains) -> do
                                    case requirement of
                                        "time" -> do 
                                            let op1_path = showPath (showResult (V.toList buses) origin_str dest_str)
                                                op2_path = showPath (showResult (V.toList trains) origin_str dest_str)
                                            putStrLn (unlines $ snd (comparePaths op1_path op2_path))
                                        "price" -> do
                                            let op1_path = showCostPath (showCostResult (V.toList buses) origin_str dest_str)
                                                op2_path = showCostPath (showCostResult (V.toList trains) origin_str dest_str)
                                            putStrLn (unlines $ snd (compareCostPaths op1_path op2_path))
                "combined" -> do
                    file1 <- prompt "Path of file 1:\n"
                    res1 <- readTransportCsv file1
                    case res1 of
                        Left err -> putStrLn err
                        Right (_, buses) -> do
                            file2 <- prompt "Path of file 2:\n"
                            res2 <- readTransportCsv file2
                            case res2 of
                                Left err -> putStrLn err
                                Right (_, trains) -> do
                                    case requirement of
                                        "time" -> do
                                            let b_list = V.toList buses
                                            let t_list = V.toList trains
                                            putStrLn (unlines $ snd (showPath (showResult (b_list ++ t_list) origin_str dest_str)))
                                        "price" -> do
                                            let b_list = V.toList buses
                                            let t_list = V.toList trains
                                            putStrLn (unlines $ snd (showCostPath (showCostResult (b_list ++ t_list) origin_str dest_str)))
                    