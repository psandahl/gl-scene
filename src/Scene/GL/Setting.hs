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
    , DepthFunction (..)
    , Face (..)
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
    | DepthFunc !DepthFunction
    -- ^ Setting the function for depth comparison (default: Less)
    | CullFaceMode !Face
    -- ^ Setting the facet cull mode (default: Back).
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
    | CullFace
    deriving (Generic, NFData, Show)

instance ToGLenum Capability where
    toGLenum DepthTest = GL.GL_DEPTH_TEST
    toGLenum CullFace  = GL.GL_CULL_FACE

-- | Depth functions.
data DepthFunction
    = Never
    -- ^ Never passes.
    | Less
    -- ^ Passes if the incoming depth value is less than the stored depth value.
    | Equal
    -- ^ Passes if the incoming depth value is equal to the stored depth value.
    | LessOrEqual
    -- ^ Passes if the incoming depth value is less than or equal to the stored depth value.
    | Greater
    -- ^ Passes if the incoming depth value is greater than the stored depth value.
    | NotEqual
    -- ^ Passes if the incoming depth value is not equal to the stored depth value.
    | GreaterOrEqual
    -- ^ Passes if the incoming depth value is greater than or equal to the stored depth value.
    | Always
    -- ^ Always pass.
    deriving (Generic, NFData, Show)

instance ToGLenum DepthFunction where
    toGLenum Never          = GL.GL_NEVER
    toGLenum Less           = GL.GL_LESS
    toGLenum Equal          = GL.GL_EQUAL
    toGLenum LessOrEqual    = GL.GL_LEQUAL
    toGLenum Greater        = GL.GL_GREATER
    toGLenum NotEqual       = GL.GL_NOTEQUAL
    toGLenum GreaterOrEqual = GL.GL_GEQUAL
    toGLenum Always         = GL.GL_ALWAYS

-- | Specification of faces to cull. Front face is a face with vertices
-- rendered in counter clock-wise order.
data Face
    = Back
    | Front
    | FrontAndBack
    deriving (Generic, NFData, Show)

instance ToGLenum Face where
    toGLenum Back         = GL.GL_BACK
    toGLenum Front        = GL.GL_FRONT
    toGLenum FrontAndBack = GL.GL_FRONT_AND_BACK

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

        DepthFunc func ->
            GL.glDepthFunc <| toGLenum func

        CullFaceMode face ->
            GL.glCullFace <| toGLenum face

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

        -- Just fetch the current depth function, and set it in the
        -- reverse setting.
        DepthFunc _ -> do
            val <- getEnum GL.GL_DEPTH_FUNC
            return <| GL.glDepthFunc val

        -- Just fetch the current cull face mode, and set it in the
        -- reverse setting.
        CullFaceMode _ -> do
            val <- getEnum GL.GL_CULL_FACE_MODE
            return <| GL.glCullFace val

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

getInteger :: GL.GLenum -> IO GL.GLint
getInteger enum =
    withArray [0] $ \ptr -> do
        GL.glGetIntegerv enum ptr
        head <$> peekArray 1 ptr

getEnum :: GL.GLenum -> IO GL.GLenum
getEnum enum = fromIntegral <$> getInteger enum
