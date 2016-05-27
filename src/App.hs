{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
-- | Module: App
--
-- Application functions
-------------------------------------------------------------------------------

#if defined(darwin_HOST_OS)
#define PLATFORM_APP_MODULE OSXApp
#else
#define PLATFORM_APP_MODULE OtherApp
#endif

module App
    ( module PLATFORM_APP_MODULE
    , module WindowOptions
    ) where

import PLATFORM_APP_MODULE
import WindowOptions
