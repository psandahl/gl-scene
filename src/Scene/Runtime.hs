-- |
-- Module: Scene.Runtime
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- The Runtime module manages the renderer's runtime data.
module Scene.Runtime
    ( Runtime (..)
    , getViewport
    , setViewport
    , getRenderState
    , emitEvent
    ) where

import           Control.Concurrent.STM (TQueue, TVar, atomically, readTVarIO,
                                         writeTQueue)
import           Control.DeepSeq        (($!!))
import           Data.IORef             (IORef, readIORef, writeIORef)
import           Graphics.UI.GLFW       (Window)
import           Scene.Types            (Event, RenderState, Viewport)

data Runtime = Runtime
    { window      :: !Window
    , viewport    :: !(IORef Viewport)
    , frameStart  :: !Double
    , renderState :: !(TVar RenderState)
    , eventQueue  :: !(TQueue Event)
    }

-- | Get the 'Viewport' value.
getViewport :: Runtime -> IO Viewport
getViewport = readIORef . viewport
{-# INLINE getViewport #-}

-- | Set a new 'Viewport' value.
setViewport :: Runtime -> Viewport -> IO ()
setViewport runtime = writeIORef (viewport runtime)
{-# INLINE setViewport #-}

-- | Get the current 'RenderState' value.
getRenderState :: Runtime -> IO RenderState
getRenderState = readTVarIO . renderState
{-# INLINE getRenderState #-}

-- | Emit an 'Event' to the event channel. Make sure that the 'Event' is
-- fully evaluated in the renderer thread.
emitEvent :: Runtime -> Event -> IO ()
emitEvent runtime event =
    atomically $ writeTQueue (eventQueue runtime) $!! event
{-# INLINE emitEvent #-}
