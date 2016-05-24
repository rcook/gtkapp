{-
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

    app <- applicationNew

    on app blockTermination $ do
        putStrLn "blockTermination"
        return False

    on app willTerminate $ do
        putStrLn "willTerminate"
        return ()

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

    applicationSetUseQuartsAccelerators app True
    applicationReady app

    -- All Gtk+ applications must have a main loop. Control ends here
    -- and waits for an event to occur (like a key press or mouse event).
    -- This function returns if the program should finish.
    mainGUI

-}
module Main (main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.OSX
import Graphics.UI.Gtk.OSX.Application
import System.Glib.GObject (objectNew)

createMenuBar descr
    = do bar <- menuBarNew
         mapM_ (createMenu bar) descr
         return bar
    where
      createMenu bar (name,items)
          = do menu <- menuNew
               item <- menuItemNewWithLabelOrMnemonic name
               menuItemSetSubmenu item menu
               menuShellAppend bar item
               mapM_ (createMenuItem menu) items
      createMenuItem menu (name,action)
          = do item <- menuItemNewWithLabelOrMnemonic name
               menuShellAppend menu item
               case action of
                 Just act -> on item menuItemActivated act
                 Nothing  -> on item menuItemActivated (return ())
      menuItemNewWithLabelOrMnemonic name
          | elem '_' name = menuItemNewWithMnemonic name
          | otherwise     = menuItemNewWithLabel name

menuBarDescr
    = [ ("_File", [ ("Open", Nothing)
                  , ("Save", Nothing)
                  , ("_Quit", Just mainQuit)
                  ]
        )
      , ("Help",  [ ("_Help", Nothing)
                  ]
        )
      ]

main =
    do initGUI
       app <- applicationNew
       window <- windowNew
       menuBar <- createMenuBar menuBarDescr
       set window [ windowTitle := "Demo"
                  , containerChild := menuBar
                  ]
       on window objectDestroy mainQuit
       --onDestroy window mainQuit
       widgetShowAll window
       widgetHide menuBar
       applicationSetMenuBar app menuBar
       applicationReady app
       mainGUI
