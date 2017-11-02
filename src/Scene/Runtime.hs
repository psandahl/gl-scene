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
    , getCurrentScene
    , getRenderState
    , emitEvent
    ) where

import           Codec.Picture          (DynamicImage)
import           Control.Concurrent.STM (TQueue, TVar, atomically, readTVarIO,
                                         writeTQueue)
import           Control.DeepSeq        (($!!))
import           Data.ByteString.Char8  (ByteString)
import           Data.IORef             (IORef, readIORef, writeIORef)
import           Graphics.UI.GLFW       (Window)
import           Scene.GL.Mesh          (Mesh, MeshRequest)
import           Scene.GL.Program       (Program, ProgramRequest)
import           Scene.GL.Texture       (Texture, TextureRequest)
import           Scene.Logger           (Logger)
import           Scene.Scene            (Scene)
import           Scene.Types            (Event, RenderState, Subscription,
                                         Viewport)

data Runtime = Runtime
    { window            :: !Window
    , logger            :: !Logger
    , viewport          :: !(IORef Viewport)
    , frameStart        :: !Double
    , currentScene      :: !(TVar Scene)
    , renderState       :: !(TVar RenderState)
    , subscriptionQueue :: !(TQueue Subscription)
    , eventQueue        :: !(TQueue Event)
    , programRequest    :: !(TQueue (ProgramRequest ByteString))
    , programReply      :: !(TQueue (Either String Program))
    , meshRequest       :: !(TQueue MeshRequest)
    , meshReply         :: !(TQueue Mesh)
    , textureRequest    :: !(TQueue (TextureRequest DynamicImage))
    , textureReply      :: !(TQueue Texture)
    }

-- | Get the 'Viewport' value.
getViewport :: Runtime -> IO Viewport
getViewport = readIORef . viewport
{-# INLINE getViewport #-}

-- | Set a new 'Viewport' value.
setViewport :: Runtime -> Viewport -> IO ()
setViewport runtime = writeIORef (viewport runtime)
{-# INLINE setViewport #-}

-- | Get the current 'Scene'.
getCurrentScene :: Runtime -> IO Scene
getCurrentScene = readTVarIO . currentScene
{-# INLINE getCurrentScene #-}

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
