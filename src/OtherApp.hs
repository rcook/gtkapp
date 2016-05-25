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
    ) where

import qualified Graphics.UI.Gtk as Gtk

data Application = MkApplication

blockTermination :: Gtk.Signal Application (IO Bool)
blockTermination = undefined

-- | Initialize application
-- Perform application initialization for non-Mac OS X platforms
initApp :: IO Application
initApp = return MkApplication

willTerminate :: Gtk.Signal Application (IO ())
willTerminate = undefined
