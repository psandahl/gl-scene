{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |
-- Module: Scene.GL.Program
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.GL.Program
    ( Program
    , ProgramRequest (..)
    , ShaderType (..)
    , readSources
    , fromRequest
    , setUniforms
    , enable
    , disable
    , delete
    ) where

import           Control.DeepSeq       (NFData)
import           Control.Exception     (SomeException, try)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Text             (Text)
import           Flow                  ((<|))
import           Foreign               (Ptr, nullPtr, peek, with)
import           Foreign.C             (peekCString, withCString)
import           GHC.Generics          (Generic)
import qualified Graphics.GL           as GL
import           Scene.GL.Types        (ToGLenum (..))
import           Scene.GL.Uniform      (UniformLocationMap, UniformValue,
                                        getUniformLocations, setUniformValues)
import           Text.Printf           (printf)

-- | A representation of a program. The 'Program' type is opaque to the user.
data Program = Program
    { programId        :: !ProgramId
    , uniformLocations :: !UniformLocationMap
    } deriving (Generic, NFData, Show)

-- | A request for a 'Program' by specifying the program's shaders and the
-- names of the program's uniforms. A program's attributes are mapped
-- directly from the VAO and are not visible at this stage.
data ProgramRequest a = ProgramRequest
    { shaders      :: ![(ShaderType, a)]
    , uniformNames :: ![Text]
    } deriving (Generic, NFData, Show)

-- | Specification of a shader type.
data ShaderType
    = Vertex
    | Fragment
    deriving (Generic, NFData, Show)

instance ToGLenum ShaderType where
    toGLenum Vertex   = GL.GL_VERTEX_SHADER
    toGLenum Fragment = GL.GL_FRAGMENT_SHADER

type ProgramId = GL.GLuint

-- | Read the sources for the 'ProgramRequest', and produce a new
-- 'ProgramRequest' with the contents of the specified files.
readSources :: ProgramRequest FilePath
            -> IO (Either String (ProgramRequest ByteString))
readSources request = do
    result <- sequence <$> mapM readShaderSource (shaders request)
    either (return . Left)
           (\shaders' -> return $ Right request { shaders = shaders' }) result

-- | Create a 'Program' from the 'ProgramRequest'.
fromRequest :: ProgramRequest ByteString -> IO (Either String Program)
fromRequest request = do
    eProgram <- fromByteStrings $ shaders request
    case eProgram of

        Right program -> do
            uniformLocations' <- getUniformLocations program <| uniformNames request
            return $ Right
                Program
                    { programId = program
                    , uniformLocations = uniformLocations'
                    }

        Left err -> return $ Left err

-- | Set uniform values for the program. Program must be enabled.
setUniforms :: Program -> [UniformValue] -> IO ()
setUniforms program = setUniformValues (uniformLocations program)
{-# INLINE setUniforms #-}

-- | Enable the program.
enable :: Program -> IO ()
enable = GL.glUseProgram . programId

-- | Disable the currently active program.
disable :: IO ()
disable = GL.glUseProgram 0

-- | Delete the program.
delete :: Program -> IO ()
delete = GL.glDeleteProgram . programId

-- | Build a shader program from bytestrings.
fromByteStrings :: [(ShaderType, ByteString)] -> IO (Either String GL.GLuint)
fromByteStrings xs = do
    eShaders <- sequence <$> mapM compileShader xs
    case eShaders of
        Right shaders' -> linkShaders shaders'
        Left err       -> return $ Left err

-- | Compile a single shader.
compileShader :: (ShaderType, ByteString)
              -> IO (Either String GL.GLuint)
compileShader (shaderType, source) = do
    shader <- GL.glCreateShader $ toGLenum shaderType
    setShaderSource shader source
    GL.glCompileShader shader

    status <- getShaderStatus $ GL.glGetShaderiv shader GL.GL_COMPILE_STATUS
    if status == GL.GL_TRUE
        then return $ Right shader
        else do
            errLog <- getInfoLog shader GL.glGetShaderInfoLog
            return $ Left (printf "%s: %s" (show shaderType) errLog)

-- | Link shaders to a program.
linkShaders :: [GL.GLuint] -> IO (Either String GL.GLuint)
linkShaders shaders' = do
    program <- GL.glCreateProgram
    mapM_ (GL.glAttachShader program) shaders'
    GL.glLinkProgram program

    status <- getShaderStatus $ GL.glGetProgramiv program GL.GL_LINK_STATUS
    if status == GL.GL_TRUE
        then do
            mapM_ (GL.glDetachShader program) shaders'
            mapM_ GL.glDeleteShader shaders'
            return $ Right program
        else do
            errLog <- getInfoLog program GL.glGetProgramInfoLog
            mapM_ GL.glDeleteShader shaders'
            GL.glDeleteProgram program
            return $ Left errLog

-- | Set the source code for the shader.
setShaderSource :: GL.GLuint -> ByteString -> IO ()
setShaderSource shader source =
    BS.useAsCString source $ \cstring ->
        with cstring $ \ptr ->
            GL.glShaderSource shader 1 ptr nullPtr

-- | Read the shader status using the provided getter.
getShaderStatus :: (Ptr GL.GLint -> IO ()) -> IO GL.GLboolean
getShaderStatus getter =
    with 0 $ \ptr -> do
        getter ptr
        v <- peek ptr
        if v == 0
            then return GL.GL_FALSE
            else return GL.GL_TRUE

-- | Read the info log using the provided getter (shader or program).
getInfoLog :: GL.GLuint
           -> (GL.GLuint -> GL.GLsizei -> Ptr GL.GLsizei -> Ptr GL.GLchar -> IO ())
           -> IO String
getInfoLog handle getter = do
    let str = Prelude.replicate 500 '\0'
    withCString str $ \ptr -> do
        getter handle 500 nullPtr ptr
        peekCString ptr

-- | Read the shader source from file.
readShaderSource :: (ShaderType, FilePath)
                 -> IO (Either String (ShaderType, ByteString))
readShaderSource (st, path) = do
    result <- tryReadFile path
    either (return . Left . show)
           (\bs -> return $ Right (st, bs)) result

tryReadFile :: FilePath -> IO (Either SomeException ByteString)
tryReadFile = try . BS.readFile
