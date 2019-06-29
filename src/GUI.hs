{-# LANGUAGE OverloadedStrings #-}

module GUI (main) where

import Graphics.UI.Gtk
import Sokoban
import Data.Maybe (catMaybes)
import Control.Monad (mapM_, forM_, forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, atomicModifyIORef, readIORef, IORef)
import Data.Text (pack)
import Prelude hiding (Either(..))

stepsString :: Maybe String
stepsString = Just "Steps: "


main :: Int -> IO ()
main levelNr = do
  initGUI
  window <- windowNew
  set window [
    windowTitle := pack "Sokoban",
    windowDefaultWidth := 500,
    windowDefaultHeight := 500 ]

  world <- loadLevel levelNr
  mapGrid <- gridNew
  prepareGrid mapGrid world

  state <- newIORef [world]
  displayWorld mapGrid state

  appGrid <- gridNew
  gridAttach appGrid mapGrid 0 0 1 1

  scoreWidget <- labelNew stepsString
  updateCounter scoreWidget state
  gridAttach appGrid scoreWidget 0 1 1 1

  containerAdd window appGrid

  -- TODO great code duplication :) good job Tomek
  window `on` keyPressEvent $ tryEvent $ do
    "Left" <- eventKeyName
    liftIO ( move Left state mapGrid )
    liftIO  ( updateCounter scoreWidget state )

  window `on` keyPressEvent $ tryEvent $ do
    "Up" <- eventKeyName
    liftIO ( move Up state mapGrid )
    liftIO  ( updateCounter scoreWidget state )

  window `on` keyPressEvent $ tryEvent $ do
    "Down" <- eventKeyName
    liftIO ( move Down state mapGrid )
    liftIO  ( updateCounter scoreWidget state )

  window `on` keyPressEvent $ tryEvent $ do
    "Right" <- eventKeyName
    liftIO ( move Right state mapGrid )
    liftIO  ( updateCounter scoreWidget state )

  window `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "z" <- eventKeyName
    liftIO ( undo state)
    liftIO  ( updateCounter scoreWidget state )
    liftIO (updateDisplay mapGrid state)

  on window objectDestroy mainQuit
  widgetShowAll window
  mainGUI


updateCounter :: Label -> IORef [World] -> IO()
updateCounter textBox state = do
  currentState <- readIORef state
  let currentWorld = head currentState
  let steps = mSteps currentWorld
  set textBox [labelText := "Score: " ++ show steps]

removeWorld :: [World] -> [World]
removeWorld [] = []
removeWorld [w] = [w]
removeWorld (w:rest) = rest

undo :: IORef[World] -> IO()
undo state = do
  currentState <- readIORef state
  atomicModifyIORef state (\s -> ((removeWorld s) , ()))

move :: Input -> IORef [World] -> Grid -> IO ()
move input state grid = do
  currentWorlds <- readIORef state
  let newWorld = modifyWorld (head currentWorlds) input
  when (newWorld /= (head currentWorlds)) $ atomicModifyIORef state (\w -> (newWorld : currentWorlds, ()))
  updateDisplay grid state


prepareGrid :: Grid -> World -> IO()
prepareGrid grid world = do
  let (maxX, maxY) = mMax world
  let coordinates = concat [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]
  forM_ coordinates (\c-> attach grid c)


attach :: Grid -> Coord -> IO ()
attach grid (x,y) = do
    image <- imageNew
    gridAttach grid image x y 1 1


updateImage :: Grid -> Coord -> IO Pixbuf -> IO ()
updateImage grid (x, y) buff = do

  maybeWidget <- gridGetChildAt grid x y
  case maybeWidget of
    Just w -> do
      let image = castToImage w
      aa <- buff
      imageSetFromPixbuf image aa


displayWorld :: Grid -> IORef [World] -> IO ()
displayWorld grid worlds = do
  ws <- readIORef worlds
  let w = head ws
  let (maxX, maxY) = mMax w
  let coordinates = concat [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]
  let toUpdate = map (\coord -> (getImageBuffer w coord, coord)) coordinates
  forM_ toUpdate (\(p,c) -> updateImage grid c p)


updateDisplay :: Grid -> IORef [World] -> IO ()
updateDisplay grid worlds = do
    ws <- readIORef worlds
    let w = head ws
    let (maxX, maxY) = mMax w
    let coordinates = concat [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]
    let toUpdate = map (\coord -> (getImageBuffer w coord, coord)) coordinates
    let toUpdateFiltered = filter (\(pix, c) -> isUpdatable w c ) toUpdate
    forM_ toUpdateFiltered (\(p,c) -> updateImage grid c p)

-- updating only squared that are around the worker - nothing else can posibly move
isUpdatable :: World -> Coord -> Bool
isUpdatable w (x,y) = abs( x - wx ) <= 2 && abs (y - wy) <= 2
  where (wx, wy) = mWorker w


getImageBuffer:: World -> Coord -> IO Pixbuf
getImageBuffer world coord =
  case () of () | isCrate world coord && isStorage world coord -> getBoxOnStorageImage
                | isWorker world coord && isStorage world coord -> getWorkerImage
                | isWall world coord -> getWallImage
                | isCrate world coord -> getBoxImage
                | isWorker world coord -> getWorkerImage
                | isStorage world coord -> getStorageImage
                | otherwise -> getEmptyImage
    where getImage fileName = pixbufNewFromFileAtSize fileName 40 40
          getWorkerImage = getImage (pack "images/penguin.png")
          getBoxImage = getImage (pack "images/crate.png")
          getWallImage = getImage (pack "images/wall.png")
          getStorageImage = getImage (pack "images/dot.png")
          getEmptyImage = getImage (pack "images/empty.png")
          getBoxOnStorageImage = getImage (pack "images/crateStorage.png")