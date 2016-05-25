module Helpers
    ( showDialog
    ) where

import Control.Exception
import Control.Monad
import Graphics.UI.Gtk

showDialog :: Window -> String -> String -> IO ()
showDialog window title message = bracket
    (messageDialogNew (Just window) [] MessageInfo ButtonsOk message)
    widgetDestroy
    (\d -> do
        set d [ windowTitle := title ]
        void $ dialogRun d)
