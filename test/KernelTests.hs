module KernelTests
    ( smokeTest
    ) where

import           Scene
import           Test.HUnit

smokeTest :: Assertion
smokeTest = do
    res <- viewScenes defaultConfiguration onInit onEvent onExit
    case res of
        Right () -> return ()
        Left err -> assertBool err False
    where
        onInit :: Viewer -> IO Int
        onInit = undefined

        onEvent :: Viewer -> Event -> Int -> IO Int
        onEvent = undefined

        onExit :: Viewer -> Int -> IO ()
        onExit = undefined
