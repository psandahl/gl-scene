-- Module: Scene.Kernel
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Kernel
    ( Configuration (..)
    , viewScenes
    ) where

import           Scene.Event  (Event (..))
import           Scene.Viewer (Viewer (..))

data Configuration = Configuration

viewScenes :: Configuration
           -> (Viewer -> IO a)
           -> (Viewer -> Event -> a -> IO a)
           -> (Viewer -> a -> IO ())
           -> IO (Either String ())
viewScenes configuration onInit onEvent onExit = undefined
