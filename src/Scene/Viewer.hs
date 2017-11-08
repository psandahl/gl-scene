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
    , sceneLog
    , close
    , programFromFiles
    , programFromByteStrings
    , meshFromRequest
    , textureFromRequest
    , waitOnTermination
    , setScene
    , subscribeKeyboard
    , unsubscribeKeyboard
    , subscribeMouseButton
    , unsubscribeMouseButton
    , subscribeCursurPos
    , unsubscribeCursorPos
    , setRenderState
    , getRenderState
    , getNextEvent
    ) where

import           Codec.Picture            (DynamicImage, readImage)
import           Control.Concurrent.Async (Async, wait)
import           Control.Concurrent.STM   (TQueue, TVar, atomically, readTQueue,
                                           readTVarIO, writeTQueue, writeTVar)
import           Control.DeepSeq          (($!!))
import           Data.ByteString.Char8    (ByteString)
import           Flow                     ((<|))
import           Scene.GL.Mesh            (Mesh, MeshRequest,
                                           hasNonEmptyVectors)
import           Scene.GL.Program         (Program, ProgramRequest, readSources)
import           Scene.GL.Texture         (Texture,
                                           TextureRequest (textureSource))
import           Scene.Logger             (LogStr, Logger, infoLog,
                                           uncheckedLog)
import           Scene.Scene              (Scene)
import           Scene.Types              (Event, RenderState (..),
                                           Subscription (..))
import           Text.Printf              (printf)

-- | The viewer record is a handle from the application to the runtime of
-- the viewer library. To the user the record is opaque.
data Viewer = Viewer
    { renderThread      :: !(Async ())
    , logger            :: !Logger
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

-- | Output a 'LogStr' to the logger.
sceneLog :: Viewer -> LogStr -> IO ()
sceneLog = uncheckedLog . logger
{-# INLINE sceneLog #-}

-- | Request the renderer to close. This state change is unconditional, when
-- the application call this the application will stop. Period.
close :: Viewer -> IO ()
close viewer = do
    infoLog (logger viewer) "gl-scene 'close' called."
    setRenderState viewer Closing

-- | Set a new 'Scene'.
setScene :: Viewer -> Scene -> IO ()
setScene viewer scene =
    atomically <| writeTVar (currentScene viewer) $!! scene
{-# INLINE setScene #-}

-- | Subscribe to keyboard events.
subscribeKeyboard :: Viewer -> IO ()
subscribeKeyboard viewer =
    addSubscription viewer SubKeyboard
{-# INLINE subscribeKeyboard #-}

-- | Unsubscribe to keyboard events.
unsubscribeKeyboard :: Viewer -> IO ()
unsubscribeKeyboard viewer =
    addSubscription viewer UnsubKeyboard
{-# INLINE unsubscribeKeyboard #-}

-- | Subscribe to mouse button events.
subscribeMouseButton :: Viewer -> IO ()
subscribeMouseButton viewer =
    addSubscription viewer SubMouseButton
{-# INLINE subscribeMouseButton #-}

-- | Unsubscribe to mouse button events.
unsubscribeMouseButton :: Viewer -> IO ()
unsubscribeMouseButton viewer =
    addSubscription viewer UnsubMouseButton
{-# INLINE unsubscribeMouseButton #-}

-- | Subscribe to cursor pos events.
subscribeCursurPos :: Viewer -> IO ()
subscribeCursurPos viewer =
    addSubscription viewer SubCursorPos
{-# INLINE subscribeCursurPos #-}

-- | Unsubscribe to cursor pos events.
unsubscribeCursorPos :: Viewer -> IO ()
unsubscribeCursorPos viewer =
    addSubscription viewer UnsubCursorPos
{-# INLINE unsubscribeCursorPos #-}

-- | Load a 'Program' from source files. All file i/o is performed in the
-- application thread.
programFromFiles :: Viewer -> ProgramRequest FilePath
                 -> IO (Either String Program)
programFromFiles viewer request = do
    infoLog (logger viewer) <|
        printf "gl-scene 'programFromFiles' called with=%s" (show request)
    result <- readSources request
    case result of
        Right newRequest ->
            programFromByteStrings viewer newRequest

        Left err         -> return $ Left err

-- | Load a 'Program' from 'ByteString's.
programFromByteStrings :: Viewer -> ProgramRequest ByteString
                       -> IO (Either String Program)
programFromByteStrings viewer request = do
    atomically <| writeTQueue (programRequest viewer) $!! request
    atomically <| readTQueue (programReply viewer)

-- | Construct a 'Mesh' from a 'MeshRequest'.
meshFromRequest :: Viewer -> MeshRequest -> IO (Either String Mesh)
meshFromRequest viewer request = do
    infoLog (logger viewer) "gl-scene 'meshFromRequest' called."
    if hasNonEmptyVectors request
        then do
            atomically <| writeTQueue (meshRequest viewer) $!! request
            Right <$> (atomically <| readTQueue (meshReply viewer))
        else return $ Left "MeshRequest must not have empty vectors"

-- | Load a 'Texture' from a 'TextureRequest'. All file i/o is performed in
-- the application thread.
textureFromRequest :: Viewer -> TextureRequest FilePath
                   -> IO (Either String Texture)
textureFromRequest viewer request = do
    infoLog (logger viewer) <|
        printf "gl-scene 'textureFromRequest' called with=%s" (show request)
    result <- readImage (textureSource request)
    case result of
        Right image -> do
            atomically <| writeTQueue (textureRequest viewer) $!!
                request { textureSource = image }
            Right <$> (atomically <| readTQueue (textureReply viewer))

        Left err -> return $ Left err

-- | Wait until the render thread has terminated.
waitOnTermination :: Viewer -> IO ()
waitOnTermination = wait . renderThread

-- | Set a new 'RenderState' value. Make sure the value is fully evaluated
-- in the calling thread.
setRenderState :: Viewer -> RenderState -> IO ()
setRenderState viewer renderState' =
    atomically <| writeTVar (renderState viewer) $!! renderState'
{-# INLINE setRenderState #-}

-- | Get the current 'RenderState' value.
getRenderState :: Viewer -> IO RenderState
getRenderState = readTVarIO . renderState
{-# INLINE getRenderState #-}

-- | Get the next 'Event' from the queue.
getNextEvent :: Viewer -> IO Event
getNextEvent viewer =
    atomically <| readTQueue (eventQueue viewer)
{-# INLINE getNextEvent #-}

addSubscription :: Viewer -> Subscription -> IO ()
addSubscription viewer sub = do
    infoLog (logger viewer) <|
        printf "gl-scene: adding subscription=%s" (show sub)
    atomically <| writeTQueue (subscriptionQueue viewer) $!! sub
