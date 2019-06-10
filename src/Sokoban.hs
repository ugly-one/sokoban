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
        loadLevel,
        world,
        simpleWorld
    ) where

import Data.List (sort, delete)
import Prelude hiding (Either(..))
import Control.Monad (forM_)

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

world2 :: [String]
world2 = [
    "    #####          ",
    "    #   #          ",
    "    #o  #          ",
    "  ###  o##         ",
    "  #  o o #         ",
    "### # ## #   ######",
    "#   # ## #####  ..#",
    "# o  o          ..#",
    "##### ### #@##  ..#",
    "    #     #########",
    "    #######        "
    ]


simpleWorld = loadLevel basicWorldString
world = loadLevel world2

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

loadLevel :: [String] -> World
loadLevel str = do
    let coordinates = [[(x,y) | x <- [0..]] | y <- [0..]]
    let elements = concat $ zipWith zip coordinates str
    let world = foldl updateWorld emptyWorld elements
    world {mMax = fst (last elements)}


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

