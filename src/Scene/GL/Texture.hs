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
    ( TextureFormat (..)
    , TextureMagFilter (..)
    , TextureMinFilter (..)
    , TextureWrap (..)
    , TextureRequest (..)
    ) where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)
import qualified Graphics.GL     as GL

-- | Texture format; RGB or RGBA.
data TextureFormat
    = RGB
    | RGBA
    deriving (Eq, Generic, NFData, Show)

data TextureMagFilter
    = MagFilterLinear
    | MagFilterNearest
    deriving (Eq, Generic, NFData, Show)

data TextureMinFilter
    = MinFilterLinear
    | MinFilterNearest
    | MinFilterNearestMipmapNearest
    | MinFilterLinearMipmapNearest
    | MinFilterNearestMipmapLinear
    | MinFilterLinearMipmapLinear
    deriving (Eq, Generic, NFData, Show)

data TextureWrap
    = WrapRepeat
    | WrapClampToBorder
    | WrapClampToEdge
    deriving (Eq, Generic, NFData, Show)

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
