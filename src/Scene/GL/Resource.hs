-- |
-- Module: Scene.GL.Resource
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Resource
    ( genVertexArray
    , genBuffer
    ) where

import           Foreign     (Ptr, peekArray, withArray)
import           Graphics.GL as GL

-- | Generate a vertex array object (VAO).
genVertexArray :: IO GL.GLuint
genVertexArray = genResource GL.glGenVertexArrays

-- | Generate a buffer object.
genBuffer :: IO GL.GLuint
genBuffer = genResource GL.glGenBuffers

-- | OpenGL resources are allocated as arrays or the resource. This helper
-- function just makes single resources.
genResource :: (GL.GLsizei -> Ptr GL.GLuint -> IO ()) -> IO GL.GLuint
genResource ctor =
    withArray [0] $ \ptr -> do
        ctor 1 ptr
        head <$> peekArray 1 ptr
