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
    ( SceneGraph (..)
    , Scene (..)
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

-- | The SceneGraph record is the root of the scene graph.
data SceneGraph = SceneGraph
    { sceneGraphSettings :: ![Setting]
    -- ^ The 'Setting's for the complete graph. E.g. clearing of the
    -- default framebuffer.

    , firstScene         :: !(Maybe Scene)
    -- ^ The first 'Scene' of the graph.
    } deriving (Generic, NFData, Show)

-- | The Scene record, specyfing stuff for the scene to be rendered.
data Scene = Scene
    { sceneRenderBuffer :: !(Maybe Framebuffer)
    -- ^ Optionally give a 'Framebuffer' target for the scene. If Nothing the
    -- default framebuffer will be used.

    , sceneSettings     :: ![Setting]
    -- ^ 'Setting's for the scene.

    , sceneEntities     :: ![Entity]
    -- ^ Entities to be rendered within the scene.

    , nextScene         :: !(Maybe Scene)
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

-- | Render the 'SceneGraph'.
render :: Viewport -> SceneGraph -> IO ()
render viewport sceneGraph = do
    setViewport viewport
    withTemporarySettings (sceneGraphSettings sceneGraph) $
        renderScenes viewport <| firstScene sceneGraph

-- | Render a single 'Scene'. Three different runtime alternatives ...
renderScenes :: Viewport -> Maybe Scene -> IO ()
renderScenes viewport (Just scene)

    -- 1. We have a specific 'Framebuffer' to render to.
    | isJust <| sceneRenderBuffer scene = do
        let fb = fromJust <| sceneRenderBuffer scene
        Framebuffer.enable fb
        setViewport <| framebufferViewport fb
        withTemporarySettings (sceneSettings scene) $
            renderEntities <| sceneEntities scene
        Framebuffer.disable
        renderScenes viewport <| nextScene scene

    -- 2. Render stuff to the default framebuffer.
    | otherwise = do
        setViewport viewport
        withTemporarySettings (sceneSettings scene) $
            renderEntities <| sceneEntities scene
        renderScenes viewport <| nextScene scene

-- 3. No more 'Scene's to process.
renderScenes _viewport Nothing = return ()

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
