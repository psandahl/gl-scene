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
    , delVertexArray
    , delBuffer
    , genTexture
    , delTexture
    ) where

import           Foreign     (Ptr, peekArray, withArray)
import           Graphics.GL as GL

-- | Generate a vertex array object (VAO).
genVertexArray :: IO GL.GLuint
genVertexArray = genResource GL.glGenVertexArrays

-- | Delete a vertex array object (VAO).
delVertexArray :: GL.GLuint -> IO ()
delVertexArray = delResource GL.glDeleteVertexArrays

-- | Generate a buffer object.
genBuffer :: IO GL.GLuint
genBuffer = genResource GL.glGenBuffers

-- | Delete a buffer object.
delBuffer :: GL.GLuint -> IO ()
delBuffer = delResource GL.glDeleteBuffers

-- | Generate a texture object.
genTexture :: IO GL.GLuint
genTexture = genResource GL.glGenTextures

-- | Delete a texture object.
delTexture :: GL.GLuint -> IO ()
delTexture = delResource GL.glDeleteTextures

-- | OpenGL resources are allocated as arrays or the resource. This helper
-- function just makes single resources.
genResource :: (GL.GLsizei -> Ptr GL.GLuint -> IO ()) -> IO GL.GLuint
genResource ctor =
    withArray [0] $ \ptr -> do
        ctor 1 ptr
        head <$> peekArray 1 ptr

-- | Helper function the delete one resource.
delResource :: (GL.GLsizei -> Ptr GL.GLuint -> IO ()) -> GL.GLuint -> IO ()
delResource dtor resource =
    withArray [resource] $ dtor 1
