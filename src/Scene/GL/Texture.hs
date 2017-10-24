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
    , TextureFormat (..)
    , TextureMagFilter (..)
    , TextureMinFilter (..)
    , TextureWrap (..)
    , TextureRequest (..)
    ) where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)
import qualified Graphics.GL     as GL

-- | A representation of a texture. The 'Texture' type is opaque to the user.
data Texture = Texture
    { textureId :: !GL.GLuint
    } deriving (Eq, Generic, NFData, Show)

-- | Texture format; RGB or RGBA.
data TextureFormat
    = RGB
    | RGBA
    deriving (Eq, Generic, NFData, Show)

-- | Magnification filter.
data TextureMagFilter
    = MagFilterLinear
    | MagFilterNearest
    deriving (Eq, Generic, NFData, Show)

-- | Minimization filter.
data TextureMinFilter
    = MinFilterLinear
    | MinFilterNearest
    | MinFilterNearestMipmapNearest
    | MinFilterLinearMipmapNearest
    | MinFilterNearestMipmapLinear
    | MinFilterLinearMipmapLinear
    deriving (Eq, Generic, NFData, Show)

-- | Texture wrap strategies.
data TextureWrap
    = WrapRepeat
    | WrapClampToBorder
    | WrapClampToEdge
    deriving (Eq, Generic, NFData, Show)

-- | A request for loading a 'Texture'.
data TextureRequest a = TextureRequest
    { format        :: !TextureFormat
    , minFilter     :: !TextureMinFilter
    , magFilter     :: !TextureMagFilter
    , wrapS         :: !TextureWrap
    , wrapT         :: !TextureWrap
    , genMipmaps    :: !Bool
    , lodBias       :: !GL.GLfloat
    , textureSource :: !a
    } deriving (Eq, Generic, NFData, Show)
