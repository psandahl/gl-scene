-- |
-- Module: Scene.Scanner
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Scanner
    ( scanRequests
    ) where

import           Codec.Picture          (DynamicImage)
import           Control.Concurrent.STM (atomically, tryReadTQueue, writeTQueue)
import           Control.DeepSeq        (($!!))
import           Data.ByteString.Char8  (ByteString)
import           Flow                   ((<|))
import qualified Scene.Callback         as Callback
import           Scene.GL.Framebuffer   (FramebufferRequest)
import qualified Scene.GL.Framebuffer   as Framebuffer
import           Scene.GL.Mesh          (MeshRequest)
import qualified Scene.GL.Mesh          as Mesh
import           Scene.GL.Program       (ProgramRequest)
import qualified Scene.GL.Program       as Program
import           Scene.GL.Texture       (TextureRequest)
import qualified Scene.GL.Texture       as Texture
import           Scene.Runtime          (Runtime (..))
import           Scene.Types            (Subscription)


-- | Scan all the request queues, and handle one request per queue and frame.
scanRequests :: Runtime -> IO ()
scanRequests runtime = do
    maybe (return ()) (handleProgramRequest runtime) =<<
        (atomically <| tryReadTQueue (programRequest runtime))

    maybe (return ()) (handleMeshRequest runtime) =<<
        (atomically <| tryReadTQueue (meshRequest runtime))

    maybe (return ()) (handleTextureRequest runtime) =<<
        (atomically <| tryReadTQueue (textureRequest runtime))

    maybe (return ()) (handleFramebufferRequest runtime) =<<
        (atomically <| tryReadTQueue (framebufferRequest runtime))

    maybe (return ()) (handleSubscriptionRequest runtime) =<<
        (atomically <| tryReadTQueue (subscriptionQueue runtime))

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

handleFramebufferRequest :: Runtime -> FramebufferRequest -> IO ()
handleFramebufferRequest runtime request = do
    result <- Framebuffer.fromRequest request
    atomically <| writeTQueue (framebufferReply runtime) $!! result

handleSubscriptionRequest :: Runtime -> Subscription -> IO ()
handleSubscriptionRequest =
    Callback.subscriptionRequest
