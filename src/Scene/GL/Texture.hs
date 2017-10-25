{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |
-- Module: Scene.GL.Texture
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Texture
    ( Texture
    , TextureBinding (..)
    , TextureFormat (..)
    , TextureMagFilter (..)
    , TextureMinFilter (..)
    , TextureWrap (..)
    , TextureRequest (..)
    , defaultTextureRequest
    , fromRequest
    , enable
    , disable
    , delete
    ) where

import           Codec.Picture        (DynamicImage, Image (..), Pixel,
                                       PixelRGB8, PixelRGBA8, convertRGB8,
                                       convertRGBA8, imageData)
import           Codec.Picture.Extra  (flipHorizontally)
import           Control.DeepSeq      (NFData)
import           Control.Monad        (when)
import qualified Data.Vector.Storable as Vector
import           Flow                 ((<|))
import           Foreign              (castPtr)
import           GHC.Generics         (Generic)
import qualified Graphics.GL          as GL
import           Scene.GL.Resource    (delTexture, genTexture)
import           Scene.GL.Types       (ToGLint (..))

-- | A representation of a texture. The 'Texture' type is opaque to the user.
data Texture = Texture
    { textureId   :: !GL.GLuint
    , textureType :: !GL.GLenum
    } deriving (Eq, Generic, NFData, Show)

-- | A binding between a 'Texture' and a texture unit. A texture unit is the value
-- you also give to the shader sampler.
data TextureBinding = TextureBinding
    { texture :: !Texture
    , unit    :: !GL.GLint
    } deriving (Eq, Generic, NFData, Show)

-- | Texture format; RGB or RGBA.
data TextureFormat
    = RGB8
    | RGBA8
    deriving (Eq, Generic, NFData, Show)

-- | Magnification filter.
data TextureMagFilter
    = MagLinear
    | MagNearest
    deriving (Eq, Generic, NFData, Show)

instance ToGLint TextureMagFilter where
    toGLint MagLinear  = GL.GL_LINEAR
    toGLint MagNearest = GL.GL_NEAREST

-- | Minimization filter.
data TextureMinFilter
    = MinLinear
    | MinNearest
    | MinNearestMipmapNearest
    | MinLinearMipmapNearest
    | MinNearestMipmapLinear
    | MinLinearMipmapLinear
    deriving (Eq, Generic, NFData, Show)

instance ToGLint TextureMinFilter where
    toGLint MinLinear               = GL.GL_LINEAR
    toGLint MinNearest              = GL.GL_NEAREST
    toGLint MinNearestMipmapNearest = GL.GL_NEAREST_MIPMAP_NEAREST
    toGLint MinLinearMipmapNearest  = GL.GL_LINEAR_MIPMAP_NEAREST
    toGLint MinNearestMipmapLinear  = GL.GL_NEAREST_MIPMAP_LINEAR
    toGLint MinLinearMipmapLinear   = GL.GL_LINEAR_MIPMAP_LINEAR

-- | Texture wrap strategies.
data TextureWrap
    = Repeat
    | ClampToBorder
    | ClampToEdge
    deriving (Eq, Generic, NFData, Show)

instance ToGLint TextureWrap where
    toGLint Repeat        = GL.GL_REPEAT
    toGLint ClampToBorder = GL.GL_CLAMP_TO_BORDER
    toGLint ClampToEdge   = GL.GL_CLAMP_TO_EDGE

-- | A request for loading a 'Texture'.
data TextureRequest a = TextureRequest
    { format        :: !TextureFormat
    , minFilter     :: !TextureMinFilter
    , magFilter     :: !TextureMagFilter
    , wrapS         :: !TextureWrap
    , wrapT         :: !TextureWrap
    , genMipmaps    :: !Bool
    , lodBias       :: !GL.GLfloat
    , flipTexture   :: !Bool
    , textureSource :: !a
    } deriving (Eq, Generic, NFData, Show)

-- | Default values for texture request parameters.
defaultTextureRequest :: FilePath -> TextureRequest FilePath
defaultTextureRequest path =
    TextureRequest
        { format = RGB8
        , minFilter = MinNearestMipmapLinear
        , magFilter = MagLinear
        , wrapS = Repeat
        , wrapT = Repeat
        , genMipmaps = True
        , lodBias = 0
        , flipTexture = True
        , textureSource = path
        }

-- | Create a 'Texture' from a 'TextureRequest'.
fromRequest :: TextureRequest DynamicImage -> IO Texture
fromRequest request = do
    tex <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D tex
    loadTexture2D request

    when (genMipmaps request) $
        GL.glGenerateMipmap GL.GL_TEXTURE_2D

    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S (toGLint <| wrapS request)
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T (toGLint <| wrapT request)
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (toGLint <| minFilter request)
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (toGLint <| magFilter request)
    GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_LOD_BIAS <| lodBias request

    GL.glBindTexture GL.GL_TEXTURE_2D 0
    return Texture { textureId = tex, textureType = GL.GL_TEXTURE_2D }

-- | Enable a 'Texture' using the binding.
enable :: TextureBinding -> IO ()
enable binding = do
    GL.glActiveTexture <| GL.GL_TEXTURE0 + fromIntegral (unit binding)
    GL.glBindTexture (textureType <| texture binding) (textureId <| texture binding)

-- | Disable a 'Texture' using the binding.
disable :: TextureBinding -> IO ()
disable binding = do
    GL.glActiveTexture <| GL.GL_TEXTURE0 + fromIntegral (unit binding)
    GL.glBindTexture (textureType <| texture binding) 0

-- | Delete the 'Texture'.
delete :: Texture -> IO ()
delete = delTexture . textureId

loadTexture2D :: TextureRequest DynamicImage -> IO ()
loadTexture2D request =
    case format request of
        RGB8 ->
            loadTexture2DRGB8 (flipTexture request) <|
                convertRGB8 (textureSource request)

        RGBA8 ->
            loadTexture2DRGBA8 (flipTexture request) <|
                convertRGBA8 (textureSource request)

loadTexture2DRGB8 :: Bool -> Image PixelRGB8 -> IO ()
loadTexture2DRGB8 flipTexture' image =
    Vector.unsafeWith (imageData <| flipImage flipTexture' image) $
        GL.glTexImage2D GL.GL_TEXTURE_2D 0 GL.GL_RGB
                        (fromIntegral <| imageWidth image)
                        (fromIntegral <| imageHeight image) 0
                        GL.GL_RGB GL.GL_UNSIGNED_BYTE . castPtr

loadTexture2DRGBA8 :: Bool -> Image PixelRGBA8 -> IO ()
loadTexture2DRGBA8 flipTexture' image =
    Vector.unsafeWith (imageData <| flipImage flipTexture' image) $
        GL.glTexImage2D GL.GL_TEXTURE_2D 0 GL.GL_RGBA
                        (fromIntegral <| imageWidth image)
                        (fromIntegral <| imageHeight image) 0
                        GL.GL_RGBA GL.GL_UNSIGNED_BYTE . castPtr

flipImage :: Pixel a => Bool -> Image a -> Image a
flipImage True  = flipHorizontally
flipImage False = id
