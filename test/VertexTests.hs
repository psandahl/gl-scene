module VertexTests
    ( withPosSizeOf
    , withPosAlignment
    , withPosEncodeDecode
    , withPosTexSizeOf
    , withPosTexAlignment
    , withPosTexEncodeDecode
    ) where

import           Foreign                             (Storable (..), peek, with)
import           Linear                              (V2 (..), V3 (..))
import qualified Scene.GL.Attribute.VertexWithPos    as WithPos
import qualified Scene.GL.Attribute.VertexWithPosTex as WithPosTex
import           Test.HUnit                          (Assertion, (@=?))

withPosSizeOf :: Assertion
withPosSizeOf =
    12 @=? sizeOf withPosSample

withPosAlignment :: Assertion
withPosAlignment =
    4 @=? alignment withPosSample

withPosEncodeDecode :: Assertion
withPosEncodeDecode = do
    transformed <- encodeDecode withPosSample
    withPosSample @=? transformed

withPosTexSizeOf :: Assertion
withPosTexSizeOf =
    20 @=? sizeOf withPosTexSample

withPosTexAlignment :: Assertion
withPosTexAlignment =
    4 @=? alignment withPosTexSample

withPosTexEncodeDecode :: Assertion
withPosTexEncodeDecode = do
    transformed <- encodeDecode withPosTexSample
    withPosTexSample @=? transformed

withPosSample :: WithPos.Vertex
withPosSample =
    WithPos.Vertex { WithPos.position = V3 1 2 3 }

withPosTexSample :: WithPosTex.Vertex
withPosTexSample =
    WithPosTex.Vertex { WithPosTex.position = V3 1 2 3
                      , WithPosTex.texCoord = V2 4 5
                      }

encodeDecode :: Storable a => a -> IO a
encodeDecode value = with value peek
