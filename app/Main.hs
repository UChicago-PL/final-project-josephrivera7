{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Csv
import System.IO
import System.Environment ( getArgs )
import Data.List ( find, minimumBy )
import Data.Ord ( comparing )
import Control.Monad ( unless )
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
    , price :: Double
    } deriving (Show)

type Graph = [(Node, [Bus])]

instance FromNamedRecord Bus where
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
readTransportCsv filePath = do
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

-- ==========================================
-- Traversing Graph via Quickest Arrival Time
-- ==========================================

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
    foldl (relaxBusTime arrivalTime) (dist, parent, queue) validBuses
  where
    buses = M.findWithDefault [] city adj
    validBuses = filter (\b -> departure b >= arrivalTime) buses

relaxBusTime :: Int -> ( M.Map String Int, M.Map String Bus, S.Set (Int, String)) -> Bus 
    -> ( M.Map String Int, M.Map String Bus, S.Set (Int, String))
relaxBusTime currentTime (distAcc, parentAcc, queueAcc) bus
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

-- ========================================
-- Traversing Graph via Cheapest Total Cost
-- ========================================

initStateCost :: String -> Int
    -> ( M.Map String Double, M.Map String Bus, S.Set String, S.Set (Double, Int, String))
initStateCost start startTime = ( M.singleton start 0.0, M.empty, S.empty, S.singleton (0.0, startTime, start))

dijkstraLoopCost :: M.Map String [Bus] -> String -> M.Map String Double -> M.Map String Bus
  -> S.Set String -> S.Set (Double, Int, String) -> Maybe (Double, [Bus])
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
processCityCost adj city cost arrivalTime dist parent queue =
    foldl (relaxBusCost cost arrivalTime) (dist, parent, queue) validBuses
  where
    buses = M.findWithDefault [] city adj
    validBuses = filter (\b -> departure b >= arrivalTime) buses

relaxBusCost :: Double -> Int -> ( M.Map String Double, M.Map String Bus, S.Set (Double, Int, String))
  -> Bus -> ( M.Map String Double, M.Map String Bus, S.Set (Double, Int, String))
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
reconstructCostPath parent goal =
    reverse (build goal)
  where
    build city =
        case M.lookup city parent of
            Nothing  -> []
            Just bus -> bus : build (origin bus)

costAwareDijkstra :: Graph -> String -> String -> Int -> Maybe (Double, [Bus])
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
showResult b origin dest = timeAwareDijkstra graph origin dest 0 where
    nodes = createNodes(findNodes b)
    graph = createGraph nodes b

showPath :: Maybe (Int, [Bus]) -> (Int, [String])
showPath Nothing = (-1, [])
showPath (Just (d, buses)) = (d, [bus_number b | b <- buses])

comparePaths :: (Int, [String]) -> (Int, [String]) -> (Int, [String])
comparePaths op1 op2 = minimumBy (comparing fst) [op1, op2]

showCostResult :: [Bus] -> String -> String -> Maybe (Double, [Bus])
showCostResult b origin dest = costAwareDijkstra graph origin dest 0 where
    nodes = createNodes(findNodes b)
    graph = createGraph nodes b
    
showCostPath :: Maybe (Double, [Bus]) -> (Double, [String])
showCostPath Nothing = (-1, [])
showCostPath (Just (d, buses)) = (d, [bus_number b | b <- buses])

compareCostPaths :: (Double, [String]) -> (Double, [String]) -> (Double, [String])
compareCostPaths op1 op2 = minimumBy (comparing fst) [op1, op2]

-- ==============
-- Main Interface
-- ============== 

prompt :: String -> IO String
prompt msg = putStr msg *> hFlush stdout *> getLine

looping :: String -> [String] -> IO String
looping p options = do
    str <- prompt p
    check str
  where
    check s
        | s `elem` options = return s
        | otherwise = looping p options

main :: IO ()
main = do 
    rstr <- looping "What do you care about most? (time or price)\n" ["time", "price"]
    ostr <- prompt "Where are you starting from?\n"
    dstr <- prompt "Where is your destination?\n"
    nstr <- looping "How many modes of transport do you want to use? (one or two)\n" ["one", "two"]
    case nstr of
        "one" -> do
            file1 <- prompt "Path of file:\n"
            res1 <- readTransportCsv file1
            case res1 of
                Left err -> putStrLn err
                Right (_, buses) -> do
                    case rstr of 
                        "time" -> print (showPath (showResult (V.toList buses) ostr dstr))
                        "price" -> print (showCostPath (showCostResult (V.toList buses) ostr dstr))
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
                                    case rstr of
                                        "time" -> do 
                                            let op1_path = showPath (showResult (V.toList buses) ostr dstr)
                                                op2_path = showPath (showResult (V.toList trains) ostr dstr)
                                            print (comparePaths op1_path op2_path)
                                        "price" -> do
                                            let op1_path = showCostPath (showCostResult (V.toList buses) ostr dstr)
                                                op2_path = showCostPath (showCostResult (V.toList trains) ostr dstr)
                                            print (compareCostPaths op1_path op2_path)
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
                                    case rstr of
                                        "time" -> do
                                            let b_list = V.toList buses
                                            let t_list = V.toList trains
                                            print (showPath (showResult (b_list ++ t_list) ostr dstr))
                                        "price" -> do
                                            let b_list = V.toList buses
                                            let t_list = V.toList trains
                                            print (showCostPath (showCostResult (b_list ++ t_list) ostr dstr))
                    