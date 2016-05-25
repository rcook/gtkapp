{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
-- | Module: App
--
-- Application functions
-------------------------------------------------------------------------------

#if defined(darwin_HOST_OS)

module App (module OSXApp) where

import OSXApp

#else

module App (module OtherApp) where

import OtherApp

#endif
