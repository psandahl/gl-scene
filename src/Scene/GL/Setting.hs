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
    , BlendEquation (..)
    , BlendFunction (..)
    , BufferBit (..)
    , Capability (..)
    , DepthFunction (..)
    , Face (..)
    , PolygonMode (..)
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
    = SetClearColor !GL.GLfloat !GL.GLfloat !GL.GLfloat !GL.GLfloat
    -- ^ Setting the color used for clearing the framebuffer (default: 0 0 0 0).
    | SetClearDepth !GL.GLfloat
    -- ^ Setting the depth value used for clearing the framebuffer (default: 1).
    | Clear ![BufferBit]
    -- ^ Clear the specified buffers identified by 'BufferBit'.
    | Enable !Capability
    -- ^ Enable a 'Capability'.
    | Disable !Capability
    -- ^ Disable a 'Capability'.
    | SetDepthMask !Bool
    -- ^ Specify whether the depth buffer is enabled for writing (default: True).
    | SetDepthFunc !DepthFunction
    -- ^ Setting the function for depth comparison (default: Less)
    | SetCullFace !Face
    -- ^ Setting the facet cull mode (default: Back).
    | SetPolygonMode !Face !PolygonMode
    -- ^ Setting the polygon mode (default: Fill).
    | SetBlendEquationSeparate !BlendEquation !BlendEquation
    -- ^ Setting the blending equation, rgb and alpha separate.
    | SetBlendFuncSeparate !BlendFunction !BlendFunction !BlendFunction !BlendFunction
    -- ^ Setting the blending function, Srgb, Drgb, Salpha, and Dalpha separate.
    deriving (Eq, Generic, NFData, Show)

-- | Buffer bits.
data BufferBit
    = ColorBufferBit
    | DepthBufferBit
    deriving (Eq, Generic, NFData, Show)

instance ToGLbitfield BufferBit where
    toGLbitfield ColorBufferBit = GL.GL_COLOR_BUFFER_BIT
    toGLbitfield DepthBufferBit = GL.GL_DEPTH_BUFFER_BIT

-- | GL capabilities.
data Capability
    = DepthTest
    | CullFace
    | Blend
    | ClipDistance !Int
    deriving (Eq, Generic, NFData, Show)

instance ToGLenum Capability where
    toGLenum DepthTest        = GL.GL_DEPTH_TEST
    toGLenum CullFace         = GL.GL_CULL_FACE
    toGLenum Blend            = GL.GL_BLEND
    toGLenum (ClipDistance i) = GL.GL_CLIP_DISTANCE0 + fromIntegral i

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
    deriving (Eq, Generic, NFData, Show)

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
-- rendered in counter clock-wise order. Back is OpenGL's initial value.
data Face
    = Back
    | Front
    | FrontAndBack
    deriving (Eq, Generic, NFData, Show)

instance ToGLenum Face where
    toGLenum Back         = GL.GL_BACK
    toGLenum Front        = GL.GL_FRONT
    toGLenum FrontAndBack = GL.GL_FRONT_AND_BACK

-- | Specify how to render polygons. Fill is OpenGL's initial mode.
data PolygonMode
    = Point
    | Line
    | Fill
    deriving (Eq, Generic, NFData, Show)

instance ToGLenum PolygonMode where
    toGLenum Point = GL.GL_POINT
    toGLenum Line  = GL.GL_LINE
    toGLenum Fill  = GL.GL_FILL

-- | Specify blend equation for blending.
data BlendEquation
    = FuncAdd
    | FuncSubtract
    | FuncReverseSubtract
    | Min
    | Max
    deriving (Eq, Generic, NFData, Show)

instance ToGLenum BlendEquation where
    toGLenum FuncAdd             = GL.GL_FUNC_ADD
    toGLenum FuncSubtract        = GL.GL_FUNC_SUBTRACT
    toGLenum FuncReverseSubtract = GL.GL_FUNC_REVERSE_SUBTRACT
    toGLenum Min                 = GL.GL_MIN
    toGLenum Max                 = GL.GL_MAX

-- | Specify blend function for blending.
data BlendFunction
    = Zero
    | One
    | SrcColor
    | OneMinusSrcColor
    | DstColor
    | OneMinusDstColor
    | SrcAlpha
    | OneMinusSrcAlpha
    | DstAlpha
    | OneMinusDstAlpha
    deriving (Eq, Generic, NFData, Show)

instance ToGLenum BlendFunction where
    toGLenum Zero             = GL.GL_ZERO
    toGLenum One              = GL.GL_ONE
    toGLenum SrcColor         = GL.GL_SRC_COLOR
    toGLenum OneMinusSrcColor = GL.GL_ONE_MINUS_SRC_COLOR
    toGLenum DstColor         = GL.GL_DST_COLOR
    toGLenum OneMinusDstColor = GL.GL_ONE_MINUS_DST_COLOR
    toGLenum SrcAlpha         = GL.GL_SRC_ALPHA
    toGLenum OneMinusSrcAlpha = GL.GL_ONE_MINUS_SRC_ALPHA
    toGLenum DstAlpha         = GL.GL_DST_ALPHA
    toGLenum OneMinusDstAlpha = GL.GL_ONE_MINUS_DST_ALPHA

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
        SetClearColor r g b a ->
            GL.glClearColor r g b a

        SetClearDepth v ->
            GL.glClearDepthf v

        Clear bufferBits ->
            GL.glClear <| concatBufferBits bufferBits

        Enable cap ->
            GL.glEnable <| toGLenum cap

        Disable cap ->
            GL.glDisable <| toGLenum cap

        SetDepthMask val ->
            GL.glDepthMask <| toGLboolean val

        SetDepthFunc func ->
            GL.glDepthFunc <| toGLenum func

        SetCullFace face ->
            GL.glCullFace <| toGLenum face

        SetPolygonMode face polygonMode ->
            GL.glPolygonMode (toGLenum face) (toGLenum polygonMode)

        SetBlendEquationSeparate rgb alpha ->
            GL.glBlendEquationSeparate (toGLenum rgb) (toGLenum alpha)

        SetBlendFuncSeparate srgb drgb salpha dalpha ->
            GL.glBlendFuncSeparate (toGLenum srgb) (toGLenum drgb)
                                   (toGLenum salpha) (toGLenum dalpha)

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
        SetClearColor {} ->
            return emptyReverseSetting

        -- ClearDepth have no reverse setting.
        SetClearDepth _ ->
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
        SetDepthMask _ -> do
            val <- getBoolean GL.GL_DEPTH_WRITEMASK
            return <| GL.glDepthMask val

        -- Just fetch the current depth function, and set it in the
        -- reverse setting.
        SetDepthFunc _ -> do
            val <- getEnum GL.GL_DEPTH_FUNC
            return <| GL.glDepthFunc val

        -- Just fetch the current cull face mode, and set it in the
        -- reverse setting.
        SetCullFace _ -> do
            val <- getEnum GL.GL_CULL_FACE_MODE
            return <| GL.glCullFace val

        -- Just fetch the current polygon mode values, and set them in the
        -- reverse setting.
        SetPolygonMode face _ -> do
            -- TODO: I belive there's a bug in the GL driver. Both values
            -- returned are describing the mode, and none of the values are
            -- describing the face.
            (_val1, val2) <- getTwoEnums GL.GL_POLYGON_MODE
            return <| GL.glPolygonMode (toGLenum face) val2

        -- | Just fetch the current blend equations, and set them in the
        -- reverse setting.
        SetBlendEquationSeparate {} -> do
            rgb <- getEnum GL.GL_BLEND_EQUATION_RGB
            alpha <- getEnum GL.GL_BLEND_EQUATION_ALPHA
            return <| GL.glBlendEquationSeparate rgb alpha

        -- | Just fetch the current blend functions, and set them in the
        -- reverse setting.
        SetBlendFuncSeparate {} -> do
            srgb <- getEnum GL.GL_BLEND_SRC_RGB
            drgb <- getEnum GL.GL_BLEND_DST_RGB
            salpha <- getEnum GL.GL_BLEND_SRC_ALPHA
            dalpha <- getEnum GL.GL_BLEND_DST_ALPHA
            return <| GL.glBlendFuncSeparate srgb drgb salpha dalpha

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

getTwoIntegers :: GL.GLenum -> IO (GL.GLint, GL.GLint)
getTwoIntegers enum =
    withArray [0, 0] $ \ptr -> do
        GL.glGetIntegerv enum ptr
        [v1, v2] <- peekArray 2 ptr
        return (v1, v2)

getEnum :: GL.GLenum -> IO GL.GLenum
getEnum enum = fromIntegral <$> getInteger enum

getTwoEnums :: GL.GLenum -> IO (GL.GLenum, GL.GLenum)
getTwoEnums enum = do
    (v1, v2) <- getTwoIntegers enum
    return (fromIntegral v1, fromIntegral v2)
