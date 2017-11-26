{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |
-- Module: Scene.GL.Framebuffer
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Framebuffer
    ( Framebuffer (colorTexture, depthTexture)
    , FramebufferRequest (..)
    , fromRequest
    , enable
    , disable
    , delete
    ) where

import           Control.DeepSeq   (NFData)
import           Flow              ((<|))
import           Foreign           (nullPtr)
import           GHC.Generics      (Generic)
import qualified Graphics.GL       as GL
import           Scene.GL.Resource (delFramebuffer, genFramebuffer, genTexture)
import           Scene.GL.Texture  (Texture (..))
import qualified Scene.GL.Texture  as Texture
import           Text.Printf       (printf)

-- | A framebuffer with color and depth attachments as 'Texture's.
data Framebuffer = Framebuffer
    { framebufferId :: !GL.GLuint
    , colorTexture  :: !Texture
    , depthTexture  :: !Texture
    } deriving (Eq, Generic, NFData, Show)

-- | A request for a new 'Framebuffer'.
data FramebufferRequest = FramebufferRequest
    { framebufferWidth  :: !Int
    , framebufferHeight :: !Int
    } deriving (Eq, Generic, NFData, Show)

-- | Create a 'Framebuffer' from the 'FramebufferRequest'.
fromRequest :: FramebufferRequest -> IO (Either String Framebuffer)
fromRequest request = do
    -- Create and bind the framebuffer object.
    fbo <- genFramebuffer
    GL.glBindFramebuffer GL.GL_FRAMEBUFFER fbo

    -- Create the color texture and attach it to the framebuffer.
    color <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D color
    GL.glTexImage2D GL.GL_TEXTURE_2D 0 GL.GL_RGB8
                    (fromIntegral <| framebufferWidth request)
                    (fromIntegral <| framebufferHeight request)
                    0 GL.GL_RGB GL.GL_UNSIGNED_BYTE nullPtr
    configureBoundTexture
    GL.glFramebufferTexture2D GL.GL_FRAMEBUFFER GL.GL_COLOR_ATTACHMENT0
                              GL.GL_TEXTURE_2D color 0

    -- Create the depth texture and attach it to the framebuffer.
    depth <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D depth
    GL.glTexImage2D GL.GL_TEXTURE_2D 0 GL.GL_DEPTH_COMPONENT
                    (fromIntegral <| framebufferWidth request)
                    (fromIntegral <| framebufferHeight request)
                    0 GL.GL_DEPTH_COMPONENT GL.GL_FLOAT nullPtr
    configureBoundTexture
    GL.glFramebufferTexture2D GL.GL_FRAMEBUFFER GL.GL_DEPTH_ATTACHMENT
                              GL.GL_TEXTURE_2D depth 0

    -- Disable buffer reading.
    GL.glReadBuffer GL.GL_NONE

    -- Set the attached color texture as draw buffer.
    GL.glDrawBuffer GL.GL_COLOR_ATTACHMENT0

    -- Read status from the bound framebuffer.
    bufferStatus <- GL.glCheckFramebufferStatus GL.GL_FRAMEBUFFER

    -- Restore default textures and framebuffer.
    GL.glBindTexture GL.GL_TEXTURE_2D 0
    GL.glBindFramebuffer GL.GL_FRAMEBUFFER 0

    -- Check status.
    if bufferStatus == GL.GL_FRAMEBUFFER_COMPLETE
        then do
            let framebuffer =
                    Framebuffer
                        { framebufferId = fbo
                        , colorTexture = Texture color GL.GL_TEXTURE_2D
                        , depthTexture = Texture depth GL.GL_TEXTURE_2D
                        }
            return <| Right framebuffer
        else do
            let err = printf "Framebuffer is not complete. Got: %d" bufferStatus
            return <| Left err

-- | Enable the 'Framebuffer' for drawing.
enable :: Framebuffer -> IO ()
enable = GL.glBindFramebuffer GL.GL_DRAW_FRAMEBUFFER . framebufferId

-- | Make sure that the default framebuffer is installed.
disable :: IO ()
disable = GL.glBindFramebuffer GL.GL_DRAW_FRAMEBUFFER 0

-- | Delete the 'Framebuffer'.
delete :: Framebuffer -> IO ()
delete framebuffer = do
    Texture.delete <| colorTexture framebuffer
    Texture.delete <| depthTexture framebuffer
    delFramebuffer <| framebufferId framebuffer

configureBoundTexture :: IO ()
configureBoundTexture = do
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S GL.GL_CLAMP_TO_EDGE
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T GL.GL_CLAMP_TO_EDGE
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER GL.GL_NEAREST
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER GL.GL_NEAREST
