{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- Module: Scene.Types
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Types
    ( Event (..)
    , RenderState (..)
    ) where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

data Event = Event

data RenderState
    = Initializing
    | Running
    | Closing
    | Done
    deriving (Generic, NFData, Show)
