{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
-- | Module: App
--
-- Platform-specific application functionality
-------------------------------------------------------------------------------

module App (initApp) where

-- Imports for GTK
import qualified Graphics.UI.Gtk as Gtk

-- Mac OS X-specific GTK imports
#if defined(darwin_HOST_OS)
import qualified Graphics.UI.Gtk.OSX as OSX
#endif

-------------------------------------------------------------------------------

#if defined(darwin_HOST_OS)

-- | Initialize application
-- Perform Mac OS X-specific application initialization
initApp :: IO ()
initApp = do
  app <- OSX.applicationNew
  menuBar <- Gtk.menuBarNew
  OSX.applicationSetMenuBar app menuBar
  OSX.applicationReady app

#else

-- | Initialize application
-- Perform application initialization for non-Mac OS X platforms
initApp :: IO ()
initApp = return ()

#endif
