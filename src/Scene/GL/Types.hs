-- |
-- Module: Scene.GL.Types
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Types
    ( ToGLenum (..)
    ) where

import qualified Graphics.GL as GL

-- | Typeclass for conversion of a high level type to 'GL.GLenum'.
class ToGLenum a where
    toGLenum :: a -> GL.GLenum
