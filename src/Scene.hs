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
    , defaultConfiguration
    , viewScenes
    ) where

import           Scene.Event  (Event (..))
import           Scene.Kernel (Configuration (..), DisplayMode (..),
                               defaultConfiguration, viewScenes)
import           Scene.Viewer (Viewer)
