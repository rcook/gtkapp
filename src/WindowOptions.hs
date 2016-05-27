-------------------------------------------------------------------------------
-- | Module: WindowOptions
--
-- Window options
-------------------------------------------------------------------------------

module WindowOptions
    ( WindowOptions(fullScreen)
    , defaultWindowOptions
    ) where

data WindowOptions = WindowOptions
    { fullScreen :: Bool
    }

defaultWindowOptions :: WindowOptions
defaultWindowOptions = WindowOptions
    { fullScreen = False
    }
