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
    , Entity (..)
    , render
    ) where

import           Control.DeepSeq  (NFData)
import           Flow             ((<|))
import           GHC.Generics     (Generic)
import qualified Graphics.GL      as GL
import           Scene.GL.Mesh    (Mesh)
import qualified Scene.GL.Mesh    as Mesh
import           Scene.GL.Program (Program)
import qualified Scene.GL.Program as Program
import           Scene.GL.Setting (Setting, withTemporarySettings)
import           Scene.GL.Uniform (UniformValue)
import           Scene.Types      (Viewport (..))

-- | The Scene record is the root of the scene graph.
data Scene = Scene
    { sceneSettings :: ![Setting]
    , sceneEntities :: ![Entity]
    } deriving (Generic, NFData, Show)

-- | The Enitity are stuff that can be rendered, or groups of stuff that
-- can be rendered.
data Entity
    = Screen
        { screenSettings :: ![Setting]
        , screenProgram  :: !Program
        , screenMesh     :: !Mesh
        , screenUniforms :: ![UniformValue]
        }
    deriving (Generic, NFData, Show)

-- | Render the 'Scene'.
render :: Viewport -> Scene -> IO ()
render viewport scene = do
    GL.glViewport 0 0 (fromIntegral <| width viewport) (fromIntegral <| height viewport)
    withTemporarySettings (sceneSettings scene) $
        mapM_ renderEntity <| sceneEntities scene

-- | Render a single 'Entity'.
renderEntity :: Entity -> IO ()
renderEntity screen@Screen {} =
    withTemporarySettings (screenSettings screen) $ do
        Program.enable <| screenProgram screen
        Program.setUniforms (screenProgram screen) (screenUniforms screen)
        Mesh.enable <| screenMesh screen
        Mesh.render <| screenMesh screen
        Mesh.disable
        Program.disable
