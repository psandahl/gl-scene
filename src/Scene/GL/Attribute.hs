-- |
-- Module: Scene.GL.Attribute
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Attribute
    ( Attribute (..)
    , pointerOffset
    ) where

import           Data.Vector.Storable (Vector)
import           Foreign              (Ptr, nullPtr, plusPtr)

-- | Type class for (vertex) attributes. The type class provide an interface
-- for copying a 'Vector' of attributes to a vertex buffer.
class Attribute a where
    setAttributes :: Vector a -> IO ()

-- | Give a byte offset expressed as a pointer.
pointerOffset :: Int -> Ptr a
pointerOffset = plusPtr nullPtr
