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
    , Viewport (..)
    ) where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

-- | Events that are emitted from the renderer.
data Event
    = CloseRequest
    | Frame !Double !Viewport
    deriving (Eq, Generic, NFData, Show)

-- | The state which the renderer can hold. It is always the application thread
-- that will change the 'RenderState'.
data RenderState
    = Initializing
    | Running
    | Closing
    | Done
    deriving (Eq, Generic, NFData, Show)

-- | The viewport size.
data Viewport = Viewport
    { width  :: !Int
    , height :: !Int
    } deriving (Eq, Generic, NFData, Show)
