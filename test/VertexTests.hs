module VertexTests
    ( withPosSizeOf
    , withPosAlignment
    , withPosEncodeDecode
    , withPosTexSizeOf
    , withPosTexAlignment
    , withPosTexEncodeDecode
    , withPosNormSizeOf
    , withPosNormAlignment
    , withPosNormEncodeDecode
    ) where

import           Foreign                              (Storable (..), peek,
                                                       with)
import           Linear                               (V2 (..), V3 (..))
import qualified Scene.GL.Attribute.VertexWithPos     as WithPos
import qualified Scene.GL.Attribute.VertexWithPosNorm as WithPosNorm
import qualified Scene.GL.Attribute.VertexWithPosTex  as WithPosTex
import           Test.HUnit                           (Assertion, (@=?))

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

withPosNormSizeOf :: Assertion
withPosNormSizeOf =
    24 @=? sizeOf withPosNormSample

withPosNormAlignment :: Assertion
withPosNormAlignment =
    4 @=? alignment withPosNormSample

withPosNormEncodeDecode :: Assertion
withPosNormEncodeDecode = do
    transformed <- encodeDecode withPosNormSample
    withPosNormSample @=? transformed

withPosSample :: WithPos.Vertex
withPosSample =
    WithPos.Vertex { WithPos.position = V3 1 2 3 }

withPosTexSample :: WithPosTex.Vertex
withPosTexSample =
    WithPosTex.Vertex { WithPosTex.position = V3 1 2 3
                      , WithPosTex.texCoord = V2 4 5
                      }

withPosNormSample :: WithPosNorm.Vertex
withPosNormSample =
    WithPosNorm.Vertex { WithPosNorm.position = V3 1 2 3
                       , WithPosNorm.normal = V3 4 5 6
                       }

encodeDecode :: Storable a => a -> IO a
encodeDecode value = with value peek
