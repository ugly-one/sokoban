module Sokoban
    (   Input(..),
        World(..),
        Coord,
        isCrate,
        isStorage,
        isWorker,
        isWall,
        isValid,
        modifyWorld,
        isFinished,
        parseLevel,
        loadLevel
    ) where

import Data.List (sort, delete)
import Prelude hiding (Either(..))
import Control.Monad (forM_)
import Data.List.Split (splitOn)

type Coord = (Int, Int)

data World = World {
    mWalls :: [Coord],
    mCrates :: [Coord],
    mStorages :: [Coord],
    mWorker :: Coord,
    mMax :: Coord,
    mSteps :: Int} deriving(Show, Eq)

data Input = Up | Down | Left | Right deriving(Show)

basicWorldString :: [String]
basicWorldString = [
    "#####",
    "#.o@#",
    "#####"
    ]

emptyWorld :: World
emptyWorld = World{
    mWalls = [],
    mCrates = [],
    mStorages = [],
    mWorker = (0,0),
    mMax = (0,0),
    mSteps = 0}

updateWorld :: World -> (Coord, Char) -> World
updateWorld world (coord, c) =
    case c of   '#' -> world {mWalls = coord:(mWalls world)}
                '@' -> world {mWorker = coord}
                'o' -> world {mCrates = coord:(mCrates world)}
                '.' -> world {mStorages = coord:(mStorages world)}
                _ -> world


modifyWorld :: World -> Input -> World
modifyWorld world input =
    if isValid world input
        then modifyWorldNoValidation world input
        else world

-- can be called only with valid input --
modifyWorldNoValidation :: World -> Input -> World
modifyWorldNoValidation world input = do
    let newWorkerPos = add (mWorker world) input
    let worldWithMovedWorker = world { mWorker = newWorkerPos, mSteps = (mSteps world) + 1 }
    if isCrate world newWorkerPos
    then worldWithMovedWorker { mCrates = (add newWorkerPos input):(delete newWorkerPos (mCrates world))}
    else
        worldWithMovedWorker


isValid :: World -> Input -> Bool
isValid world input =
    case () of
    ()  | isWall world newWorkerPos     -> False
        | isCrate world newWorkerPos    -> not (isWall world newCratePos) && not (isCrate world newCratePos)
        | isFinished world              -> False
        | otherwise                     -> True
    where
        newWorkerPos = add (mWorker world) input
        newCratePos = add newWorkerPos input


add :: Coord -> Input -> Coord
add (x,y) input =
    case input of
    Up -> (x, y-1)
    Down -> (x, y+1)
    Left -> (x-1, y)
    Right -> (x+1, y)

isWorker :: World -> Coord -> Bool
isWorker world coord = coord == mWorker world

isStorage :: World -> Coord -> Bool
isStorage world coord = elem coord (mStorages world)

isWall :: World -> Coord -> Bool
isWall world coord = elem coord (mWalls world)

isCrate :: World -> Coord -> Bool
isCrate world coord = elem coord (mCrates world)

isFinished :: World -> Bool
isFinished world = sort (mCrates world) == sort (mStorages world)

-- parsing levels
loadLevel :: Int -> IO World
loadLevel nr = do
    worldsNotParsed <- loadLevelsFromFile
    let worldToParse = worldsNotParsed !! nr
    let worldToParse2 = makeEachItemSameLength worldToParse
    let world = parseLevel worldToParse2
    return world

loadLevelsFromFile :: IO [[String]]
loadLevelsFromFile = do
    contents <- readFile "src/levels.txt"
    let worlds = (splitOn "\n\n" contents) :: [String]
    return (map (\w -> splitOn "\n" w) worlds)

parseLevel :: [String] -> World
parseLevel str = do
    let coordinates = [[(x,y) | x <- [0..]] | y <- [0..]]
    let elements = concat $ zipWith zip coordinates str
    let world = foldl updateWorld emptyWorld elements
    world {mMax = fst (last elements)}

makeEachItemSameLength :: [String] -> [String]
makeEachItemSameLength items =
    map (adjustLength desiredLength) items
    where desiredLength = findBiggestLength items

adjustLength :: Int -> String -> String
adjustLength desiredLength item =
    item ++ concat (replicate lenghtDifference " ")
    where lenghtDifference = abs (desiredLength - length item)

findBiggestLength :: [String] -> Int
findBiggestLength = foldl getLength 0

getLength :: Int -> String -> Int
getLength l value =
    if valueLength > l
        then valueLength
    else l
    where valueLength = length value

-- end of parsing levels