-- |
-- Module: Scene.Viewer
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- The Viewer module provides access to the renderer from the application
-- thread. It shares STM structures with the renderer.
module Scene.Viewer
    ( Viewer (..)
    , close
    , waitOnTermination
    , setRenderState
    , getRenderState
    , getNextEvent
    ) where

import           Control.Concurrent.Async (Async, wait)
import           Control.Concurrent.STM   (TQueue, TVar, atomically, readTQueue,
                                           readTVarIO, writeTVar)
import           Control.DeepSeq          (($!!))
import           Scene.Types              (Event, RenderState (..))

-- | The viewer record is a handle from the application to the runtime of
-- the viewer library. To the user the record is opaque.
data Viewer = Viewer
    { renderThread :: !(Async ())
    , renderState  :: !(TVar RenderState)
    , eventQueue   :: !(TQueue Event)
    }

-- | Request the renderer to close. This state change is unconditional, when
-- the application call this the application will stop. Period.
close :: Viewer -> IO ()
close viewer = setRenderState viewer Closing

-- | Wait until the render thread has terminated.
waitOnTermination :: Viewer -> IO ()
waitOnTermination = wait . renderThread

-- | Set a new 'RenderState' value. Make sure the value is fully evaluated
-- in the calling thread.
setRenderState :: Viewer -> RenderState -> IO ()
setRenderState viewer renderState' =
    atomically $ writeTVar (renderState viewer) $!! renderState'

-- | Get the current 'RenderState' value.
getRenderState :: Viewer -> IO RenderState
getRenderState = readTVarIO . renderState
{-# INLINE getRenderState #-}

-- | Get the next 'Event' from the queue.
getNextEvent :: Viewer -> IO Event
getNextEvent viewer =
    atomically $ readTQueue (eventQueue viewer)
{-# INLINE getNextEvent #-}
