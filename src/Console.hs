module Console 
    (
        main
    ) where
        
import Prelude hiding (Either(..))
import Control.Monad (forM_)
import System.IO ( stdin, stdout, hSetEcho, hSetBuffering, BufferMode(..) )
import Sokoban
import Data.List.Split (splitOn)
import Data.List (elemIndex)

getStringRepresentation :: World -> Coord -> String
getStringRepresentation world coord =
    case () of () | isCrate world coord && isStorage world coord -> "*"
                | isWorker world coord && isStorage world coord -> "+"
                | isWall world coord -> "#"
                | isCrate world coord -> "o"
                | isWorker world coord -> "@"
                | isStorage world coord -> "."
                | otherwise -> " "

displayChar :: World -> Coord -> IO()
displayChar world (x,y) = do 
    let (rightEdge, _) = mMax world
    let toPrint = getStringRepresentation world (x,y)
    if x == rightEdge 
    then putStr $ toPrint ++ "\n"
    else putStr toPrint

displayWorld :: World -> IO()
displayWorld world = do
    let (maxX, maxY) = mMax world
    let coordinates = concat [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]
    forM_ coordinates (\coord -> displayChar world coord)
  

getInput :: IO Input
getInput = do
    char <- getChar
    case char of
        'w' -> return Up
        's' -> return Down
        'a' -> return Left
        'd' -> return Right
        _ -> getInput

clear :: IO()    
clear = putStr "\ESC[2J"

gameLoop :: World -> IO()
gameLoop world = do
    clear
    displayWorld world
    input <- getInput
    let world' = modifyWorld world input
    if isFinished world'
    then clear >> print "well done" >> displayWorld world'
    else gameLoop world'

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    world <- loadLevel 0
    gameLoop world