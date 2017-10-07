-- |
-- Module: Scene
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene
    ( Configuration (..)
    , DisplayMode (..)
    , Event (..)
    , Viewer
    , Viewport (..)
    , defaultConfiguration
    , viewScenes
    , close
    ) where

import           Scene.Kernel (Configuration (..), DisplayMode (..),
                               defaultConfiguration, viewScenes)
import           Scene.Types  (Event (..), Viewport (..))
import           Scene.Viewer (Viewer, close)
