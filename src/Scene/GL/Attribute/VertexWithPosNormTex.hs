-- |
-- Module: Graphics.Scene.Attribute.VertexWithPosNormTex
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Attribute.VertexWithPosNormTex
    ( Vertex (..)
    ) where

import           Data.Vector.Storable as Vector
import           Flow                 ((<|))
import           Foreign              (Storable (..), castPtr, plusPtr)
import qualified Graphics.GL          as GL
import           Linear               (V2 (..), V3 (..))
import           Scene.GL.Attribute   (Attribute (..), pointerOffset)

-- | A vertex with three attributes; position, normal and texture coordinates.
data Vertex = Vertex
    { position :: !(V3 GL.GLfloat)
    , normal   :: !(V3 GL.GLfloat)
    , texCoord :: !(V2 GL.GLfloat)
    } deriving (Eq, Show)

-- | Storable instance.
instance Storable Vertex where
    sizeOf v = sizeOf (position v) + sizeOf (normal v) + sizeOf (texCoord v)
    alignment v = alignment <| position v
    peek ptr = do
        p <- peek <| castPtr ptr
        n <- peek <| castPtr (ptr `plusPtr` sizeOf p)
        t <- peek <| castPtr (ptr `plusPtr` (sizeOf p + sizeOf n))
        return Vertex { position = p, normal = n, texCoord = t }
    poke ptr v = do
        let pPtr = castPtr ptr
            nPtr = castPtr (pPtr `plusPtr` sizeOf (position v))
            tPtr = castPtr (nPtr `plusPtr` sizeOf (normal v))
        poke pPtr <| position v
        poke nPtr <| normal v
        poke tPtr <| texCoord v

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

        -- Texture coordinate attribute.
        GL.glEnableVertexAttribArray 2
        GL.glVertexAttribPointer 2 2 GL.GL_FLOAT GL.GL_FALSE
                                 (fromIntegral itemSize)
                                 (pointerOffset <| sizeOf (position first) + sizeOf (normal first))
