{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.Gtk
import Sokoban
import Data.Maybe (catMaybes)
import Control.Monad (mapM_, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, atomicModifyIORef, readIORef, IORef)
import Data.Text (pack)
import Prelude hiding (Either(..))

stepsString :: Maybe String
stepsString = Just "Steps: "


main :: IO ()
main = do
  state <- newIORef world
  initGUI
  window <- windowNew
  set window [
    windowTitle := pack "Sokoban",
    windowDefaultWidth := 500,
    windowDefaultHeight := 500 ]

  mapGrid <- gridNew
  prepareGrid mapGrid world

  displayWorld mapGrid (readIORef state)

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

  on window objectDestroy mainQuit
  widgetShowAll window
  mainGUI


updateCounter :: Label -> IORef World -> IO()
updateCounter textBox state = do
  w <- readIORef state
  let steps = mSteps w
  set textBox [labelText := "Score: " ++ show steps]


move :: Input -> IORef World -> Grid -> IO ()
move input state grid = do
  atomicModifyIORef state (\w -> (modifyWorldValidation w input, ()))
  updateDisplay grid (readIORef state)


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


displayWorld :: Grid -> IO World -> IO ()
displayWorld grid world = do
  w <- world
  let (maxX, maxY) = mMax w
  let coordinates = concat [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]
  let toUpdate = map (\coord -> (getImageBuffer w coord, coord)) coordinates
  forM_ toUpdate (\(p,c) -> updateImage grid c p)


updateDisplay :: Grid -> IO World -> IO ()
updateDisplay grid world = do
    w <- world
    let (maxX, maxY) = mMax w
    let coordinates = concat [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]
    let toUpdate = map (\coord -> (getImageBuffer w coord, coord)) coordinates
    let toUpdateFiltered = filter (\(pix, c) -> isUpdatable w c ) toUpdate
    forM_ toUpdateFiltered (\(p,c) -> updateImage grid c p)

-- updating only squared that are around the worker - nothing else can posibly move
isUpdatable :: World -> Coord -> Bool
isUpdatable w (x,y) = abs( x - wx ) <= 1 && abs (y - wy) <=1
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
          getWorkerImage = getImage (pack "penguin-icon.png")
          getBoxImage = getImage (pack "wooden_box_512.png")
          getWallImage = getImage (pack "brick_wall-512.png")
          getStorageImage = getImage (pack "dot.png")
          getEmptyImage = getImage (pack "empty.png")
          getBoxOnStorageImage = getImage (pack "crateStorage.png")