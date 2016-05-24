-- A simple program to demonstrate Gtk2Hs.
module Main (Main.main) where

import Control.Exception
import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.OSX

showDialog :: Window -> String -> String -> IO ()
showDialog window title message = bracket
    (messageDialogNew (Just window) [] MessageInfo ButtonsOk message)
    widgetDestroy
    (\d -> do
        set d [ windowTitle := title ]
        void $ dialogRun d)

main :: IO ()
main = do
    void initGUI

    -- Create a new window
    window <- windowNew

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

    void $ on button buttonActivated $ showDialog window "THE-TITLE" "THE-MESSAGE"

    -- Gtk+ allows several callbacks for the same event.
    -- This one will cause the window to be destroyed by calling
    -- widgetDestroy. The callbacks are called in the sequence they were added.
    void $ on button buttonActivated $ do
        putStrLn "A \"clicked\"-handler to say \"destroy\""
        widgetDestroy window

    -- Insert the hello-world button into the window.
    set window [ containerChild := button ]

    -- The final step is to display this newly created widget. Note that this
    -- also allocates the right amount of space to the windows and the button.
    widgetShowAll window

    app <- applicationNew

    -- blockTermination: return True to prevent quit, False to allow
    on app blockTermination $ do
        putStrLn "blockTermination"
        return False

    -- willTerminate: handle clean-up etc.
    on app willTerminate $ do
        putStrLn "willTerminate"

    menuBar <- menuBarNew
    applicationSetMenuBar app menuBar
    applicationReady app

    -- All Gtk+ applications must have a main loop. Control ends here
    -- and waits for an event to occur (like a key press or mouse event).
    -- This function returns if the program should finish.
    mainGUI
