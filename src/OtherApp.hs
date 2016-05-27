-------------------------------------------------------------------------------
-- | Module: OtherApp
--
-- Application functions for non-Mac OS X platforms
-------------------------------------------------------------------------------

module OtherApp
    (
      Application
    , blockTermination
    , initApp
    , willTerminate
    , windowNew
    ) where

import qualified Graphics.UI.Gtk as Gtk
import           WindowOptions

data Application = Application

blockTermination :: Gtk.Signal Application (IO Bool)
blockTermination = undefined

-- | Initialize application
-- Perform application initialization for non-Mac OS X platforms
initApp :: IO Application
initApp = return Application

willTerminate :: Gtk.Signal Application (IO ())
willTerminate = undefined

windowNew :: WindowOptions -> IO Gtk.Window
windowNew _ = Gtk.windowNew
