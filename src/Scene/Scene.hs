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
    , render
    ) where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)
import           Scene.GL.Action (Action, withTemporaryActions)

-- | The Scene record is the root of the scene graph.
data Scene = Scene
    { actions :: ![Action]
    } deriving (Generic, NFData, Show)

-- | Render the 'Scene'.
render :: Scene -> IO ()
render scene =
    withTemporaryActions (actions scene) $
        return ()
