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
    , Capability (..)
    , applyPersistantActions
    , withTemporaryActions
    ) where

import           Control.DeepSeq (NFData)
import           Data.Bits       ((.|.))
import           Data.List       (foldl')
import           Flow            ((<|))
import           GHC.Generics    (Generic)
import qualified Graphics.GL     as GL
import           Scene.GL.Types  (ToGLbitfield (..), ToGLenum (..))

-- | Actions are used to manipulate the GL state machine.
data Action
    = ClearColor !GL.GLfloat !GL.GLfloat !GL.GLfloat !GL.GLfloat
    | Clear ![BufferBit]
    | Enable !Capability
    | Disable !Capability
    deriving (Generic, NFData, Show)

-- | Buffer bits.
data BufferBit
    = ColorBufferBit
    | DepthBufferBit
    deriving (Generic, NFData, Show)

instance ToGLbitfield BufferBit where
    toGLbitfield ColorBufferBit = GL.GL_COLOR_BUFFER_BIT
    toGLbitfield DepthBufferBit = GL.GL_DEPTH_BUFFER_BIT

-- | GL capabilities.
data Capability
    = DepthTest
    deriving (Generic, NFData, Show)

instance ToGLenum Capability where
    toGLenum DepthTest = GL.GL_DEPTH_TEST

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

        Enable cap ->
            GL.glEnable <| toGLenum cap

        Disable cap ->
            GL.glDisable <| toGLenum cap

runReverseActions :: [IO ()] -> IO ()
runReverseActions = sequence_
{-# INLINE runReverseActions #-}

makeReverseActions :: [Action] -> IO [IO ()]
makeReverseActions = mapM makeReverseAction
{-# INLINE makeReverseActions #-}

-- | Make a reverse action (if any) for the given 'Action'.
makeReverseAction :: Action -> IO (IO ())
makeReverseAction action =
    case action of
        -- ClearColor have to reverse action.
        ClearColor {} ->
            return emptyReverseAction

        -- Clear have no reverse action.
        Clear {} ->
            return emptyReverseAction

        -- When enabling a 'Capability', if the capability was disabled
        -- it shall be reversed back to disabled.
        Enable cap -> do
            enabled <- GL.glIsEnabled <| toGLenum cap
            if enabled == GL.GL_TRUE
                then return emptyReverseAction
                else return <| GL.glDisable <| toGLenum cap

        -- When disabling a 'Capability', if the capability was enabled
        -- it shall be reversed back to enabled.
        Disable cap -> do
            enabled <- GL.glIsEnabled <| toGLenum cap
            if enabled == GL.GL_TRUE
                then return <| GL.glEnable <| toGLenum cap
                else return emptyReverseAction

emptyReverseAction :: IO ()
emptyReverseAction = return ()

concatBufferBits :: [BufferBit] -> GL.GLbitfield
concatBufferBits = foldl' (\acc bit -> acc .|. toGLbitfield bit) 0
{-# INLINE concatBufferBits #-}
