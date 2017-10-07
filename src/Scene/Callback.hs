-- |
-- Module: Scene.Callback
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Handle GLFW callbacks. All callbacks will run in the renderer thread.
module Scene.Callback
    ( subscribeToMandatoryCallbacks
    ) where

import           Graphics.UI.GLFW (Error, Window)
import qualified Graphics.UI.GLFW as GLFW
import           Scene.Runtime    (Runtime)
import qualified Scene.Runtime    as Runtime
import           Scene.Types      (Viewport (..))

-- | Subscribe to mandatory callbacks; error callback and window size callback.
subscribeToMandatoryCallbacks :: Runtime -> IO ()
subscribeToMandatoryCallbacks runtime = do
    GLFW.setErrorCallback $ Just (errorCallback runtime)
    GLFW.setWindowSizeCallback (Runtime.window runtime) $ Just (windowSizeCallback runtime)

errorCallback :: Runtime -> Error -> String -> IO ()
errorCallback _runtime _error _str = return ()

windowSizeCallback :: Runtime -> Window -> Int -> Int -> IO ()
windowSizeCallback runtime _window width' height' =
    Runtime.setViewport runtime Viewport { width = width', height = height' }
