{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |
-- Module: Scene.GL.Action
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- The Action module provides GL actions to manipulate the GL state machine.
module Scene.GL.Action
    ( Action (..)
    , applyPersistantActions
    , withTemporaryActions
    ) where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)
import qualified Graphics.GL     as GL

-- | Actions are used to manipulate the GL state machine.
data Action
    = ClearColor !GL.GLfloat !GL.GLfloat !GL.GLfloat !GL.GLfloat
    deriving (Generic, NFData, Show)

-- | Apply persistant actions. Shall only be used for global actions.
applyPersistantActions :: [Action] -> IO ()
applyPersistantActions = applyActions

-- | Apply temporary actions before executing the IO action. After the IO
-- action the effect of the actions are reverted.
withTemporaryActions :: [Action] -> IO () -> IO ()
withTemporaryActions glActions ioAction = do
    reverseActions <- makeReverseActions glActions
    applyActions glActions
    ioAction
    runReverseActions reverseActions

applyActions :: [Action] -> IO ()
applyActions = mapM_ applyAction

applyAction :: Action -> IO ()
applyAction action =
    case action of
        ClearColor r g b a ->
            GL.glClearColor r g b a

runReverseActions :: [IO ()] -> IO ()
runReverseActions = sequence_

makeReverseActions :: [Action] -> IO [IO ()]
makeReverseActions = mapM makeReverseAction

makeReverseAction :: Action -> IO (IO ())
makeReverseAction action =
    case action of
        ClearColor {} ->
            return emptyReverseAction

emptyReverseAction :: IO ()
emptyReverseAction = return ()
