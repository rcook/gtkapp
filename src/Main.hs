-- A simple program to demonstrate Gtk2Hs.
module Main (Main.main) where

import Control.Exception
import Control.Monad
import Graphics.UI.Gtk

import qualified App
import qualified Helpers

main :: IO ()
main = do
    void initGUI

    app <- App.initApp

    void $ on app App.blockTermination $ do
        putStrLn "blockTermination"
        return False

    void $ on app App.willTerminate $ putStrLn "willTerminate"

    -- Create a new window
    window <- App.windowNew App.defaultWindowOptions { App.fullScreen = True }

    -- Here we connect the "destroy" event to a signal handler.
    -- This event occurs when we call widgetDestroy on the window
    -- or if the user closes the window.
    void $ on window objectDestroy mainQuit

    -- Sets the border width and tile of the window. Note that border width
    -- attribute is in 'Container' from which 'Window' is derived.
    set window [ containerBorderWidth := 10, windowTitle := "Hello World" ]

    -- Creates a new button with the label "Hello World".
    button <- buttonNew
    set button [ buttonLabel := "Hello World" ]

    -- When the button receives the "clicked" signal, it will call the
    -- function given as the second argument.
    void $ on button buttonActivated (putStrLn "Hello World")

    void $ on button buttonActivated $ Helpers.showDialog window "THE-TITLE" "THE-MESSAGE"

    -- Gtk+ allows several callbacks for the same event.
    -- This one will cause the window to be destroyed by calling
    -- widgetDestroy. The callbacks are called in the sequence they were added.
    void $ on button buttonActivated $ do
        putStrLn "A \"clicked\"-handler to say \"destroy\""
        widgetDestroy window

    button2 <- buttonNew
    set button2 [ buttonLabel := "Another button" ]

    hButtonBox <- hButtonBoxNew
    mapM_ (containerAdd hButtonBox)
        [ button
        , button2
        ]

    -- Insert button box into window
    set window [ containerChild := hButtonBox ]

    -- The final step is to display this newly created widget. Note that this
    -- also allocates the right amount of space to the windows and the button.
    widgetShowAll window

    -- All Gtk+ applications must have a main loop. Control ends here
    -- and waits for an event to occur (like a key press or mouse event).
    -- This function returns if the program should finish.
    mainGUI
