{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |
-- Module: Scene.GL.Setting
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- The Action module provides GL actions to manipulate the GL state machine.
module Scene.GL.Setting
    ( Setting (..)
    , BufferBit (..)
    , Capability (..)
    , applyPersistantSettings
    , withTemporarySettings
    ) where

import           Control.DeepSeq (NFData)
import           Data.Bits       ((.|.))
import           Data.List       (foldl')
import           Flow            ((<|))
import           Foreign         (peekArray, withArray)
import           GHC.Generics    (Generic)
import qualified Graphics.GL     as GL
import           Scene.GL.Types  (ToGLbitfield (..), ToGLenum (..))

-- | Settings are used to manipulate the GL state machine.
data Setting
    = ClearColor !GL.GLfloat !GL.GLfloat !GL.GLfloat !GL.GLfloat
    -- ^ Setting the color used for clearing the framebuffer (default: 0 0 0 0).
    | ClearDepth !GL.GLfloat
    -- ^ Setting the depth value used for clearing the framebuffer (default: 1).
    | Clear ![BufferBit]
    -- ^ Clear the specified buffers identified by 'BufferBit'.
    | Enable !Capability
    -- ^ Enable a 'Capability'.
    | Disable !Capability
    -- ^ Disable a 'Capability'.
    | DepthMask !Bool
    -- ^ Specify whether the depth buffer is enabled for writing (default: True).
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

-- | Apply persistant settings. Shall only be used for global actions.
applyPersistantSettings :: [Setting] -> IO ()
applyPersistantSettings = applySettings

-- | Apply temporary settings before executing the action. After the
-- action the effect of the settings are reverted.
withTemporarySettings :: [Setting] -> IO () -> IO ()
withTemporarySettings settings action = do
    reverseSettings <- makeReverseSettings settings
    applySettings settings
    action
    runReverseSettings reverseSettings

applySettings :: [Setting] -> IO ()
applySettings = mapM_ applySetting
{-# INLINE applySettings #-}

applySetting :: Setting -> IO ()
applySetting setting =
    case setting of
        ClearColor r g b a ->
            GL.glClearColor r g b a

        ClearDepth v ->
            GL.glClearDepthf v

        Clear bufferBits ->
            GL.glClear <| concatBufferBits bufferBits

        Enable cap ->
            GL.glEnable <| toGLenum cap

        Disable cap ->
            GL.glDisable <| toGLenum cap

        DepthMask val ->
            GL.glDepthMask <| toGLboolean val

runReverseSettings :: [IO ()] -> IO ()
runReverseSettings = sequence_
{-# INLINE runReverseSettings #-}

makeReverseSettings :: [Setting] -> IO [IO ()]
makeReverseSettings = mapM makeReverseSetting
{-# INLINE makeReverseSettings #-}

-- | Make a reverse setting (if any) for the given 'Setting'.
makeReverseSetting :: Setting -> IO (IO ())
makeReverseSetting setting =
    case setting of
        -- ClearColor have no reverse setting.
        ClearColor {} ->
            return emptyReverseSetting

        -- ClearDepth have no reverse setting.
        ClearDepth _ ->
            return emptyReverseSetting

        -- Clear have no reverse setting.
        Clear {} ->
            return emptyReverseSetting

        -- When enabling a 'Capability', if the capability was disabled
        -- it shall be reversed back to disabled.
        Enable cap -> do
            enabled <- GL.glIsEnabled <| toGLenum cap
            if enabled == GL.GL_TRUE
                then return emptyReverseSetting
                else return <| GL.glDisable <| toGLenum cap

        -- When disabling a 'Capability', if the capability was enabled
        -- it shall be reversed back to enabled.
        Disable cap -> do
            enabled <- GL.glIsEnabled <| toGLenum cap
            if enabled == GL.GL_TRUE
                then return <| GL.glEnable <| toGLenum cap
                else return emptyReverseSetting

        -- Just fetch the current depth mask status, and set it in the
        -- reverse setting.
        DepthMask _ -> do
            val <- getBoolean GL.GL_DEPTH_WRITEMASK
            return <| GL.glDepthMask val

emptyReverseSetting :: IO ()
emptyReverseSetting = return ()

concatBufferBits :: [BufferBit] -> GL.GLbitfield
concatBufferBits = foldl' (\acc bit -> acc .|. toGLbitfield bit) 0

toGLboolean :: Bool -> GL.GLboolean
toGLboolean False = GL.GL_FALSE
toGLboolean True  = GL.GL_TRUE

getBoolean :: GL.GLenum -> IO GL.GLboolean
getBoolean enum =
    withArray [GL.GL_FALSE] $ \ptr -> do
        GL.glGetBooleanv enum ptr
        head <$> peekArray 1 ptr
