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
    , subscriptionRequest
    ) where

import           Flow             ((<|))
import           Graphics.UI.GLFW (Error, Key, KeyState, ModifierKeys,
                                   MouseButton, MouseButtonState, Window)
import qualified Graphics.UI.GLFW as GLFW
import           Linear           (V2 (..))
import           Scene.Logger     (ToLogStr (..), uncheckedLog)
import           Scene.Runtime    (Runtime (logger))
import qualified Scene.Runtime    as Runtime
import           Scene.Types      (Event (..), Subscription (..), Viewport (..))

-- | Subscribe to mandatory callbacks; error callback and window size callback.
subscribeToMandatoryCallbacks :: Runtime -> IO ()
subscribeToMandatoryCallbacks runtime = do
    GLFW.setErrorCallback $ Just (errorCallback runtime)
    GLFW.setWindowSizeCallback (Runtime.window runtime) $ Just (windowSizeCallback runtime)

-- | Handle a subscription request from the user.
subscriptionRequest :: Runtime -> Subscription -> IO ()
subscriptionRequest runtime subscription =
    case subscription of
        SubKeyboard   ->
            GLFW.setKeyCallback (Runtime.window runtime) <|
                Just (keyCallback runtime)

        UnsubKeyboard -> GLFW.setKeyCallback (Runtime.window runtime) Nothing

        SubMouseButton ->
            GLFW.setMouseButtonCallback (Runtime.window runtime) <|
                Just (mouseButtonCallback runtime)

        UnsubMouseButton ->
            GLFW.setMouseButtonCallback (Runtime.window runtime) Nothing

        SubCursorPos ->
            GLFW.setCursorPosCallback (Runtime.window runtime) <|
                Just (cursorPosCallback runtime)

        UnsubCursorPos ->
            GLFW.setCursorPosCallback (Runtime.window runtime) Nothing

errorCallback :: Runtime -> Error -> String -> IO ()
errorCallback runtime _error = uncheckedLog (logger runtime) . toLogStr

windowSizeCallback :: Runtime -> Window -> Int -> Int -> IO ()
windowSizeCallback runtime _window width' height' =
    Runtime.setViewport runtime Viewport { width = width', height = height' }

keyCallback :: Runtime -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback runtime _window key _ keyState modifierKeys =
    Runtime.emitEvent runtime <| KeyStroke key keyState modifierKeys

mouseButtonCallback :: Runtime -> Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
mouseButtonCallback runtime window button buttonState modifierKeys = do
    (x, y) <- GLFW.getCursorPos window
    Runtime.emitEvent runtime <|
        MouseButton button buttonState modifierKeys (V2 x y)

cursorPosCallback :: Runtime -> Window -> Double -> Double -> IO ()
cursorPosCallback runtime _window x y =
    Runtime.emitEvent runtime <| CursorPos (V2 x y)
