-- |
-- Module: Graphics.Scene.Attribute.VertexWithPos
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Attribute.VertexWithPos
    ( Vertex (..)
    ) where

import           Data.Vector.Storable as Vector
import           Foreign              (Storable (..), castPtr)
import qualified Graphics.GL          as GL
import           Linear               (V3 (..))
import           Scene.GL.Attribute   (Attribute (..), pointerOffset)

-- | A vertex with just one attribute; position.
data Vertex = Vertex
    { position :: !(V3 GL.GLfloat)
    } deriving (Eq, Show)

-- | Storable instance.
instance Storable Vertex where
    sizeOf = sizeOf . position
    alignment = alignment . position
    peek ptr = Vertex <$> peek (castPtr ptr)
    poke ptr = poke (castPtr ptr) . position

-- | Attribute instance.
instance Attribute Vertex where
    setAttributes vertices = do
        let first = Vector.head vertices
            itemSize = sizeOf first

        GL.glEnableVertexAttribArray 0
        GL.glVertexAttribPointer 0 3 GL.GL_FLOAT GL.GL_FALSE
                                (fromIntegral itemSize)
                                (pointerOffset 0)
