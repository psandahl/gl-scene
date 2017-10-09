{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Module: Scene.GL.Uniform
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Uniform
    ( UniformLocation
    , UniformLocationMap
    , Uniform (..)
    , UniformValue (..)
    , getUniformLocations
    , setUniformValues
    ) where

import           Control.DeepSeq     (NFData (..))
import           Control.Monad       (foldM)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text, unpack)
import           Flow                ((|>))
import           Foreign.C           (withCString)
import qualified Graphics.GL         as GL
import           Text.Printf         (printf)

-- | Type alias for a uniform location.
type UniformLocation = GL.GLint

type UniformLocationMap = HashMap Text UniformLocation

-- | Type class for setting of uniform values.
class Uniform a where
    setUniform :: UniformLocation -> a -> IO ()

-- | A uniform value pair of a name and its value. The name is connected
-- to location through 'UniformLocationMap' and 'setUniformValues'.
data UniformValue = forall a. (NFData a, Show a, Uniform a) => UniformValue
    { name  :: !Text
    , value :: !a
    }

instance NFData UniformValue where
    rnf (UniformValue name' value') =
        rnf name' |> const (rnf value')

instance Show UniformValue where
    show (UniformValue name' value') =
        printf "{ name=%s\n, value=%s\n}\n" (show name') (show value')

-- | Get the uniform locations from the program.
getUniformLocations :: GL.GLuint -> [Text] -> IO UniformLocationMap
getUniformLocations programId =
    foldM (\ulMap name' -> do
               loc <- getUniformLocation programId name'
               return $ HashMap.insert name' loc ulMap
          ) HashMap.empty

getUniformLocation :: GL.GLuint -> Text -> IO UniformLocation
getUniformLocation programId name' =
    withCString (unpack name') $
        GL.glGetUniformLocation programId

-- | Set the uniform values with help of the location map. The target program
-- must be active.
setUniformValues :: UniformLocationMap -> [UniformValue] -> IO ()
setUniformValues locationMap =
    mapM_ (setUniformValue locationMap)

setUniformValue :: UniformLocationMap -> UniformValue -> IO ()
setUniformValue locationMap (UniformValue name' value') =
    case HashMap.lookup name' locationMap of
        Just location ->
            setUniform location value'

        Nothing -> return ()
