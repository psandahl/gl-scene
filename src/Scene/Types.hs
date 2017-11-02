{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
    ( DisplayMode (..)
    , Event (..)
    , RenderState (..)
    , Viewport (..)
    , Subscription (..)
    ) where

import           Control.DeepSeq  (NFData)
import           GHC.Generics     (Generic)
import           Graphics.UI.GLFW (Key, KeyState, ModifierKeys)

-- | Display mode for the 'Viewer' window.
data DisplayMode
    = FullScreen
    | Windowed !Int !Int
    deriving Show

-- | Events that are emitted from the renderer.
data Event
    = CloseRequest
    | Frame !Double !Viewport
    | KeyStroke !Key !KeyState !ModifierKeys
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

-- | Subscription specification.
data Subscription
    = SubKeyboard
    | UnsubKeyboard
    deriving (Eq, Generic, NFData, Show)

-- Orphan instances.

instance NFData Key where
instance NFData KeyState where
instance NFData ModifierKeys where
