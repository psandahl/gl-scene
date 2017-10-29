-- |
-- Module: Scene.Logger
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Logger
    ( Logger
    , LogStr
    , ToLogStr (..)
    , mkLogger
    , closeLogger
    , checkedLog
    , uncheckedLog
    , infoLog
    ) where


import           Control.Monad         (when)
import           Data.Monoid           ((<>))
import           Flow                  ((<|))
import           System.Log.FastLogger

-- | Logger record.
data Logger = Logger
    { debug     :: !Bool
    , timeCache :: !(IO FormattedTime)
    , loggerSet :: !LoggerSet
    }

-- | Make a new 'Logger'. With the boolean flag tell if we run in debug mode
-- or not.
mkLogger :: Bool -> IO Logger
mkLogger debug' = do
    timeCache' <- newTimeCache simpleTimeFormat'
    loggerSet' <- newStdoutLoggerSet defaultBufSize

    return Logger { debug = debug'
                  , timeCache = timeCache'
                  , loggerSet = loggerSet'
                  }

-- | Close the logger.
closeLogger :: Logger -> IO ()
closeLogger = rmLoggerSet . loggerSet

-- | Output a 'LogStr' only if we run in debug mode.
checkedLog :: Logger -> LogStr -> IO ()
checkedLog logger logStr =
    when (debug logger) $
        uncheckedLog logger logStr

-- | Output a 'LogStr'.
uncheckedLog :: Logger -> LogStr -> IO ()
uncheckedLog logger logStr = do
    timeStamp <- timeCache logger
    pushLogStrLn (loggerSet logger) <| toLogStr timeStamp <> toLogStr ": " <> logStr
{-# INLINE uncheckedLog #-}

-- | Convenience function. Checked log, with String arguments.
infoLog :: Logger -> String -> IO ()
infoLog logger = checkedLog logger . toLogStr
{-# INLINE infoLog #-}
