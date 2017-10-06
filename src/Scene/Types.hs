{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |
-- Module: Scene.Types
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Basic types.
module Scene.Types
    ( Event (..)
    , RenderState (..)
    ) where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

-- | Events that are emitted from the renderer.
data Event
    = CloseRequest
    deriving (Generic, NFData, Show)

-- | The state which the renderer can hold. It is always the application thread
-- that will change the 'RenderState'.
data RenderState
    = Initializing
    | Running
    | Closing
    | Done
    deriving (Generic, NFData, Show)
