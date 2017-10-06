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
    , getRenderState
    , emitEvent
    ) where

import           Control.Concurrent.STM (TQueue, TVar, atomically, readTVarIO,
                                         writeTQueue)
import           Control.DeepSeq        (($!!))
import           Graphics.UI.GLFW       (Window)
import           Scene.Types            (Event, RenderState)

data Runtime = Runtime
    { window      :: !Window
    , renderState :: !(TVar RenderState)
    , eventQueue  :: !(TQueue Event)
    }

-- | Get the current 'RenderState' value.
getRenderState :: Runtime -> IO RenderState
getRenderState = readTVarIO . renderState

-- | Emit an 'Event' to the event channel. Make sure that the 'Event' is
-- fully evaluated in the renderer thread.
emitEvent :: Runtime -> Event -> IO ()
emitEvent runtime event =
    atomically $ writeTQueue (eventQueue runtime) $!! event
