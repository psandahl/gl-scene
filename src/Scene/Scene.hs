{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |
-- Module: Scene.Scene
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- The Scene module provides the scene graph data structures and the rendering
-- of the graph.
module Scene.Scene
    ( Scene (..)
    , Rendering (..)
    , Entity (..)
    , render
    ) where

import           Control.DeepSeq      (NFData)
import           Data.Maybe           (fromJust, isJust)
import           Flow                 ((<|))
import           GHC.Generics         (Generic)
import qualified Graphics.GL          as GL
import           Scene.GL.Framebuffer (Framebuffer (framebufferViewport))
import qualified Scene.GL.Framebuffer as Framebuffer
import           Scene.GL.Mesh        (Mesh)
import qualified Scene.GL.Mesh        as Mesh
import           Scene.GL.Program     (Program)
import qualified Scene.GL.Program     as Program
import           Scene.GL.Setting     (Setting, withTemporarySettings)
import           Scene.GL.Texture     (TextureBinding)
import qualified Scene.GL.Texture     as Texture
import           Scene.GL.Uniform     (UniformValue)
import           Scene.Types          (Viewport (..))

-- | The Scene record is the root of stuff to be rendered during a frame. A
-- Scene consist of zero or more 'Rendering's.
data Scene = Scene
    { sceneSettings  :: ![Setting]
    -- ^ 'Setting's that are common for the complete graph.

    , firstRendering :: !(Maybe Rendering)
    -- ^ The first 'Rendering' of the Scene.
    } deriving (Generic, NFData, Show)

-- | The Rendering record, representing one rendering pass.
data Rendering = Rendering
    { renderingBuffer   :: !(Maybe Framebuffer)
    -- ^ Optionally give a 'Framebuffer' target for the Rendering. If Nothing the
    -- default framebuffer will be used.

    , renderingSettings :: ![Setting]
    -- ^ 'Setting's for the rendering.

    , renderingEntities :: ![Entity]
    -- ^ Entities to be rendered within the scene.

    , nextRendering     :: !(Maybe Rendering)
    -- ^ The next scene to be handled.
    } deriving (Generic, NFData, Show)

-- | The Entity is stuff to be rendered.
data Entity = Entity
    { entitySettings :: ![Setting]
    -- ^ 'Setting's for the entity.

    , entityProgram  :: !Program
    -- ^ The 'Program' used by the entity.

    , entityMesh     :: !Mesh
    -- ^ The 'Mesh' used by the entity.

    , entityUniforms :: ![UniformValue]
    -- ^ The list of 'UniformValue's for the entity.

    , entityTextures :: ![TextureBinding]
    -- ^ The list of 'TextureBinding's for the entity.
    } deriving (Generic, NFData, Show)

-- | Render the 'Scene'.
render :: Viewport -> Scene -> IO ()
render viewport scene = do
    setViewport viewport
    withTemporarySettings (sceneSettings scene) $
        performRenderings viewport <| firstRendering scene

-- |  Recursively perform renderings. Three different runtime alternatives ...
performRenderings :: Viewport -> Maybe Rendering -> IO ()
performRenderings viewport (Just rendering)

    -- 1. We have a specific 'Framebuffer' to render to.
    | isJust <| renderingBuffer rendering = do
        let fb = fromJust <| renderingBuffer rendering
        Framebuffer.enable fb
        setViewport <| framebufferViewport fb
        withTemporarySettings (renderingSettings rendering) $
            renderEntities <| renderingEntities rendering
        Framebuffer.disable
        performRenderings viewport <| nextRendering rendering

    -- 2. Render stuff to the default framebuffer.
    | otherwise = do
        setViewport viewport
        withTemporarySettings (renderingSettings rendering) $
            renderEntities <| renderingEntities rendering
        performRenderings viewport <| nextRendering rendering

-- 3. No more 'Rendering's to process.
performRenderings _viewport Nothing = return ()

-- | Render all 'Entity's.
renderEntities :: [Entity] -> IO ()
renderEntities = mapM_ renderEntity

-- | Render a single 'Entity'.
renderEntity :: Entity -> IO ()
renderEntity entity@Entity {} =
    withTemporarySettings (entitySettings entity) $ do
        Program.enable <| entityProgram entity
        mapM_ Texture.enable <| entityTextures entity
        Program.setUniforms (entityProgram entity) (entityUniforms entity)
        Mesh.enable <| entityMesh entity
        Mesh.render <| entityMesh entity
        Mesh.disable
        mapM_ Texture.disable <| entityTextures entity
        Program.disable

setViewport :: Viewport -> IO ()
setViewport viewport =
    GL.glViewport 0 0 (fromIntegral <| width viewport)
                      (fromIntegral <| height viewport)
