module KernelTests
    ( smokeTest
    ) where

import           Scene
import           Test.HUnit

-- | Simple smoke test where app state and event mechanism is tested. Requires
-- OpenGL supporting device.
smokeTest :: Assertion
smokeTest = do
    res <- viewScenes (defaultConfiguration { displayMode = Windowed 800 600 })
                      onInit onEvent onExit
    case res of
        Right () -> return ()
        Left err -> assertBool err False
    where
        -- Initialize app state with 1.
        onInit :: Viewer -> IO Int
        onInit _viewer = return 1

        -- Receive Frame event. Check input and return new app state.
        onEvent :: Viewer -> Event -> Int -> IO Int
        onEvent viewer (Frame duration (Viewport w h)) val = do
            close viewer

            -- Duration must be > 0.
            assertBool "Shall be > 0" (duration > 0)

            -- Check that the viewport is the configured.
            800 @=? w
            600 @=? h

            -- Check initial app state.
            1 @=? val

            -- Set new app state.
            return 2

        -- Shall not happen.
        onEvent viewer _event _val = do
            close viewer
            assertBool "Shall not happen" False
            return 2

        -- At exit, check the app state.
        onExit :: Viewer -> Int -> IO ()
        onExit _viewer val = do
            2 @=? val
            return ()
