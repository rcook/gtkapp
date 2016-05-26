{-# LANGUAGE RecordWildCards #-}

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
    , WindowOptions(fullScreen)
    , defaultWindowOptions
    , initApp
    , windowNew
    ) where

import           Control.Monad
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OSX as OSX

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

data WindowOptions = WindowOptions
    { fullScreen :: Bool
    }

defaultWindowOptions :: WindowOptions
defaultWindowOptions = WindowOptions
    { fullScreen = False
    }

windowNew :: WindowOptions -> IO Gtk.Window
windowNew WindowOptions{..} = do
    window <- Gtk.windowNew
    when fullScreen $
        void $ Gtk.on window Gtk.realize $
        Gtk.widgetGetWindow window >>= maybe (return ()) OSX.allowFullscreen
    return window
