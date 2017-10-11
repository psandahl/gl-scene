-- |
-- Module: Scene.GL.Types
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Types
    ( ToGLenum (..)
    , ToGLbitfield (..)
    ) where

import qualified Graphics.GL as GL

-- | Typeclass for conversion of a high level type to 'GL.GLenum'.
class ToGLenum a where
    toGLenum :: a -> GL.GLenum

-- | Typeclass for conversion of high level type to 'GL.GLbitfield'.
class ToGLbitfield a where
    toGLbitfield :: a -> GL.GLbitfield
