module Main
    ( main
    ) where


import           KernelTests                    (smokeTest)
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           VertexTests                    (withPosAlignment,
                                                 withPosEncodeDecode,
                                                 withPosSizeOf,
                                                 withPosTexAlignment,
                                                 withPosTexEncodeDecode,
                                                 withPosTexSizeOf)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "Attribute tests - Storable instance"
        [ testCase "WithPos sizeOf == 12" withPosSizeOf
        , testCase "WithPos alignment == 4" withPosAlignment
        , testCase "WithPos encodeDecode" withPosEncodeDecode
        , testCase "WithPosTex sizeOf == 20" withPosTexSizeOf
        , testCase "WithPosTex alignement == 4" withPosTexAlignment
        , testCase "WithPosTex encodeDecode" withPosTexEncodeDecode
        ]
    , testGroup "Kernel tests"
        [ testCase "Kernel smoke test" smokeTest
        ]
    ]
