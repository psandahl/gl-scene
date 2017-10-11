{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |
-- Module: Scene.GL.Action
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- The Action module provides GL actions to manipulate the GL state machine.
module Scene.GL.Action
    ( Action (..)
    , BufferBit (..)
    , applyPersistantActions
    , withTemporaryActions
    ) where

import           Control.DeepSeq (NFData)
import           Data.Bits       ((.|.))
import           Data.List       (foldl')
import           Flow            ((<|))
import           GHC.Generics    (Generic)
import qualified Graphics.GL     as GL
import           Scene.GL.Types  (ToGLbitfield (..))

-- | Actions are used to manipulate the GL state machine.
data Action
    = ClearColor !GL.GLfloat !GL.GLfloat !GL.GLfloat !GL.GLfloat
    | Clear ![BufferBit]
    deriving (Generic, NFData, Show)

-- | Buffer bits.
data BufferBit
    = ColorBufferBit
    | DepthBufferBit
    deriving (Generic, NFData, Show)

instance ToGLbitfield BufferBit where
    toGLbitfield ColorBufferBit = GL.GL_COLOR_BUFFER_BIT
    toGLbitfield DepthBufferBit = GL.GL_DEPTH_BUFFER_BIT

-- | Apply persistant actions. Shall only be used for global actions.
applyPersistantActions :: [Action] -> IO ()
applyPersistantActions = applyActions

-- | Apply temporary actions before executing the IO action. After the IO
-- action the effect of the actions are reverted.
withTemporaryActions :: [Action] -> IO () -> IO ()
withTemporaryActions glActions ioAction = do
    reverseActions <- makeReverseActions glActions
    applyActions glActions
    ioAction
    runReverseActions reverseActions

applyActions :: [Action] -> IO ()
applyActions = mapM_ applyAction
{-# INLINE applyActions #-}

applyAction :: Action -> IO ()
applyAction action =
    case action of
        ClearColor r g b a ->
            GL.glClearColor r g b a

        Clear bufferBits ->
            GL.glClear <| concatBufferBits bufferBits

runReverseActions :: [IO ()] -> IO ()
runReverseActions = sequence_
{-# INLINE runReverseActions #-}

makeReverseActions :: [Action] -> IO [IO ()]
makeReverseActions = mapM makeReverseAction

makeReverseAction :: Action -> IO (IO ())
makeReverseAction action =
    case action of
        ClearColor {} ->
            return emptyReverseAction

        Clear {} ->
            return emptyReverseAction

emptyReverseAction :: IO ()
emptyReverseAction = return ()

concatBufferBits :: [BufferBit] -> GL.GLbitfield
concatBufferBits = foldl' (\acc bit -> acc .|. toGLbitfield bit) 0
{-# INLINE concatBufferBits #-}
