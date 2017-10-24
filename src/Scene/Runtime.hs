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
    , scanRequests
    ) where

import           Codec.Picture          (DynamicImage)
import           Control.Concurrent.STM (TQueue, TVar, atomically, readTVarIO,
                                         tryReadTQueue, writeTQueue)
import           Control.DeepSeq        (($!!))
import           Data.ByteString.Char8  (ByteString)
import           Data.IORef             (IORef, readIORef, writeIORef)
import           Flow                   ((<|))
import           Graphics.UI.GLFW       (Window)
import           Scene.GL.Mesh          (Mesh, MeshRequest)
import qualified Scene.GL.Mesh          as Mesh
import           Scene.GL.Program       (Program, ProgramRequest)
import qualified Scene.GL.Program       as Program
import           Scene.GL.Texture       (Texture, TextureRequest)
import qualified Scene.GL.Texture       as Texture
import           Scene.Scene            (Scene)
import           Scene.Types            (Event, RenderState, Viewport)

data Runtime = Runtime
    { window         :: !Window
    , viewport       :: !(IORef Viewport)
    , frameStart     :: !Double
    , currentScene   :: !(TVar Scene)
    , renderState    :: !(TVar RenderState)
    , eventQueue     :: !(TQueue Event)
    , programRequest :: !(TQueue (ProgramRequest ByteString))
    , programReply   :: !(TQueue (Either String Program))
    , meshRequest    :: !(TQueue MeshRequest)
    , meshReply      :: !(TQueue Mesh)
    , textureRequest :: !(TQueue (TextureRequest DynamicImage))
    , textureReply   :: !(TQueue Texture)
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

-- | Scan all the request queues, and handle one request per queue.
scanRequests :: Runtime -> IO ()
scanRequests runtime = do
    maybe (return ()) (handleProgramRequest runtime) =<<
        (atomically <| tryReadTQueue (programRequest runtime))

    maybe (return ()) (handleMeshRequest runtime) =<<
        (atomically <| tryReadTQueue (meshRequest runtime))

    maybe (return ()) (handleTextureRequest runtime) =<<
        (atomically <| tryReadTQueue (textureRequest runtime))

handleProgramRequest :: Runtime -> ProgramRequest ByteString -> IO ()
handleProgramRequest runtime request = do
    result <- Program.fromRequest request
    atomically <| writeTQueue (programReply runtime) $!! result

handleMeshRequest :: Runtime -> MeshRequest -> IO ()
handleMeshRequest runtime request = do
    result <- Mesh.fromRequest request
    atomically <| writeTQueue (meshReply runtime) $!! result

handleTextureRequest :: Runtime -> TextureRequest DynamicImage -> IO ()
handleTextureRequest runtime request = do
    result <- Texture.fromRequest request
    atomically <| writeTQueue (textureReply runtime) $!! result
