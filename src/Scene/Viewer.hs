-- Module: Scene.Viewer
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Viewer
    ( Viewer (..)
    , waitOnTermination
    ) where

import           Control.Concurrent.Async (Async, wait)
import           Control.Concurrent.STM   (TVar)
import           Scene.Types              (RenderState)

-- | The viewer record is a handle from the application to the runtime of
-- the viewer library. To the user the record is opaque.
data Viewer = Viewer
    { renderThread :: !(Async ())
    , renderState  :: !(TVar RenderState)
    }

-- | Wait until the render thread has terminated.
waitOnTermination :: Viewer -> IO ()
waitOnTermination = wait . renderThread
