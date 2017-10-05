-- Module: Scene
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene
    ( Configuration (..)
    , Event (..)
    , Viewer
    , viewScenes
    ) where

import           Scene.Event  (Event (..))
import           Scene.Kernel (Configuration (..), viewScenes)
import           Scene.Viewer (Viewer)
