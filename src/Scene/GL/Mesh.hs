{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Module: Scene.GL.Mesh
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Mesh
    ( Mesh
    , MeshRequest (..)
    , Primitive (..)
    , fromRequest
    , enable
    , disable
    , delete
    ) where

import           Control.DeepSeq      (NFData (..))
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import           Flow                 ((<|))
import           Foreign              (Storable (..), castPtr)
import           GHC.Generics         (Generic)
import qualified Graphics.GL          as GL
import           Scene.GL.Attribute   (Attribute (..))
import           Scene.GL.Resource    (delVertexArray, genBuffer,
                                       genVertexArray)
import           Scene.GL.Types       (ToGLenum (..))
import           Text.Printf          (printf)

-- | A representation of a mesh. The 'Mesh' type is opaque to the user.
data Mesh = Mesh
    { theVao       :: !GL.GLuint
    , thePrimitive :: !Primitive
    , theIndices   :: !(Vector GL.GLuint)
    } deriving (Generic, NFData, Show)

-- | A request for a 'Mesh' by specifying vertices, indices and primitive.
data MeshRequest = forall a. (Attribute a, Storable a, Show a) => MeshRequest
    { vertices  :: !(Vector a)
    , indices   :: !(Vector GL.GLuint)
    , primitive :: !Primitive
    }

instance NFData MeshRequest where
    rnf (MeshRequest vertices' indices' primitive') =
        const (rnf vertices') <|
            const (rnf indices') <|
                rnf primitive'

instance Show MeshRequest where
    show (MeshRequest vertices' indices' primitive') =
        printf "{ vertices=%s\n, indices=%s\n, primitive=%s\n}\n"
               (show vertices')
               (show indices')
               (show primitive')

-- | Primitives that a 'Mesh' can represent.
data Primitive
    = Triangles
    deriving (Generic, NFData, Show)

instance ToGLenum Primitive where
    toGLenum Triangles = GL.GL_TRIANGLES

-- | Create a 'Mesh' from a 'MeshRequest'. Assume that the vertex vector is
-- non-empty.
fromRequest :: MeshRequest -> IO Mesh
fromRequest = undefined

-- | Enable the 'Mesh' by binding it.
enable :: Mesh -> IO ()
enable = GL.glBindVertexArray . theVao
{-# INLINE enable #-}

-- | Disable the current 'Mesh'.
disable :: IO ()
disable = GL.glBindVertexArray 0
{-# INLINE disable #-}

-- | Delete the VAO related to this 'Mesh'.
delete :: Mesh -> IO ()
delete = delVertexArray . theVao

-- | Fill the VBO with vertex data. Assume that the vertex vector is non-empty.
fillVBO :: Storable a => Vector a -> IO ()
fillVBO vertices' =
    Vector.unsafeWith vertices' $ \ptr -> do
        let first = Vector.head vertices'
            itemSize = sizeOf first
            storageSize = itemSize * Vector.length vertices'
        GL.glBufferData GL.GL_ARRAY_BUFFER
                        (fromIntegral storageSize)
                        (castPtr ptr)
                        GL.GL_STATIC_DRAW

-- | Alloc one VAO, one VBO and bind both buffers.
allocBoundBuffers :: IO GL.GLuint
allocBoundBuffers = do
    vaoId <- genVertexArray
    GL.glBindVertexArray vaoId

    GL.glBindBuffer GL.GL_ARRAY_BUFFER =<< genBuffer

    return vaoId
