module GUI (main) where

import Control.Monad.IO.Class
import Graphics.UI.Gtk

main :: IO ()
main = do
    initGUI
    window <- windowNew
    set window [ windowTitle := "sokoban",
                    windowResizable := True]

    display <- entryNew
    set display [entryEditable := True, entryText := "Ha"]

    -- display2 <- entryNew
    -- set display2 [entryEditable := True, entryText := "Ha"]

    -- emptyImage <- imageNew
    -- layout <- overlayNew

    buff <- pixbufNewFromFile "penguin-icon.png"
    print "test"

    -- image <- imageNewFromPixbuf buff
    containerAdd window display

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False
    widgetShowAll window
    mainGUI 