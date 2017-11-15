-- |
-- Module: Graphics.Scene.Attribute.VertexWithPosNorm
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Attribute.VertexWithPosNorm
    ( Vertex (..)
    ) where

import           Data.Vector.Storable as Vector
import           Flow                 ((<|))
import           Foreign              (Storable (..), castPtr, plusPtr)
import qualified Graphics.GL          as GL
import           Linear               (V3 (..))
import           Scene.GL.Attribute   (Attribute (..), pointerOffset)

-- | A vertex with two attributes; position and normal.
data Vertex = Vertex
    { position :: !(V3 GL.GLfloat)
    , normal   :: !(V3 GL.GLfloat)
    } deriving (Eq, Show)

-- | Storable instance.
instance Storable Vertex where
    sizeOf v = sizeOf (position v) + sizeOf (normal v)
    alignment v = alignment <| position v
    peek ptr = do
        p <- peek <| castPtr ptr
        n <- peek <| castPtr (ptr `plusPtr` sizeOf p)
        return Vertex { position = p, normal = n }
    poke ptr v = do
        let pPtr = castPtr ptr
            nPtr = castPtr (pPtr `plusPtr` sizeOf (position v))
        poke pPtr <| position v
        poke nPtr <| normal v

-- | Attribute instance.
instance Attribute Vertex where
    setAttributes vertices = do
        let first = Vector.head vertices
            itemSize = sizeOf first

        -- Position attribute.
        GL.glEnableVertexAttribArray 0
        GL.glVertexAttribPointer 0 3 GL.GL_FLOAT GL.GL_FALSE
                                (fromIntegral itemSize)
                                (pointerOffset 0)

        -- Normal attribute.
        GL.glEnableVertexAttribArray 1
        GL.glVertexAttribPointer 1 3 GL.GL_FLOAT GL.GL_FALSE
                                 (fromIntegral itemSize)
                                 (pointerOffset <| sizeOf (position first))
