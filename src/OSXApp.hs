-------------------------------------------------------------------------------
-- | Module: OSXApp
--
-- Mac OS X-specific application functions
-------------------------------------------------------------------------------

module OSXApp
    (
      OSX.Application
    , OSX.blockTermination
    , OSX.willTerminate
    , initApp
    ) where

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OSX as OSX

-- | Initialize application
-- Perform Mac OS X-specific application initialization
initApp :: IO OSX.Application
initApp = do
    app <- OSX.applicationNew
    menuBar <- Gtk.menuBarNew
    OSX.applicationSetMenuBar app menuBar
    OSX.applicationReady app
    return app
