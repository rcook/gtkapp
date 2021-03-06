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
    , windowNew
    ) where

import           Control.Monad
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OSX as OSX
import           WindowOptions

-- | Initialize application
-- Perform Mac OS X-specific application initialization
initApp :: IO OSX.Application
initApp = do
    app <- OSX.applicationNew

    usesQuartzAccelerators <- OSX.applicationGetUseQuartzAccelerators app
    unless usesQuartzAccelerators $ do
        putStrLn "WARNING: applicationGetUseQuartzAccelerators returned False"
        OSX.applicationSetUseQuartzAccelerators app True

    menuBar <- Gtk.menuBarNew
    OSX.applicationSetMenuBar app menuBar
    OSX.applicationReady app
    return app

windowNew :: WindowOptions -> IO Gtk.Window
windowNew windowOptions = do
    window <- Gtk.windowNew
    when (fullScreen windowOptions) $
        void $ Gtk.on window Gtk.realize $
        Gtk.widgetGetWindow window >>= maybe (return ()) OSX.allowFullscreen
    return window
