-- |
-- Module: Scene.Kernel
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- The Kernel module is implementing all low level runtime stuff.
module Scene.Kernel
    ( Configuration (..)
    , defaultConfiguration
    , viewScenes
    ) where

import           Control.Concurrent       (getNumCapabilities)
import           Control.Concurrent.Async (asyncBound)
import           Control.Concurrent.STM   (newTQueueIO, newTQueueIO, newTVarIO)
import           Control.DeepSeq          (($!!))
import           Control.Monad            (unless, when)
import           Control.Monad.Except     (runExceptT, throwError)
import           Control.Monad.IO.Class   (liftIO)
import           Data.IORef               (newIORef)
import           Data.Maybe               (isNothing)
import           Flow                     ((<|))
import           GHC.Conc                 (getNumProcessors)
import           Graphics.UI.GLFW         (Window)
import           Graphics.UI.GLFW         as GLFW
import           Scene.Callback           (subscribeToMandatoryCallbacks)
import           Scene.GL.Setting         (Setting, applyPersistantSettings)
import           Scene.Logger             (Logger, closeLogger, infoLog,
                                           mkLogger)
import           Scene.Runtime            (Runtime)
import qualified Scene.Runtime            as Runtime
import           Scene.Scene              (Scene (..))
import qualified Scene.Scene              as Scene
import           Scene.Types              (DisplayMode (..), Event (..),
                                           RenderState (..), Viewport (..))
import           Scene.Viewer             (Viewer)
import qualified Scene.Viewer             as Viewer
import           Text.Printf              (printf)

-- | Configuration data for the 'Viewer' window to create.
data Configuration = Configuration
    { caption        :: !String
    , glVersionMajor :: !Int
    , glVersionMinor :: !Int
    , displayMode    :: !DisplayMode
    , globalSettings :: ![Setting]
    , initialScene   :: !Scene
    , debugContext   :: !Bool
    } deriving Show

-- | Default 'Configuration'.
defaultConfiguration :: Configuration
defaultConfiguration =
    Configuration
        { caption = "Scene Viewer"
        , glVersionMajor = 3
        , glVersionMinor = 3
        , displayMode = Windowed 1024 768
        , globalSettings = []
        , initialScene =
            Scene { sceneSettings = []
                  , sceneEntities = []
                  }
        , debugContext = True
        }

-- | Configure a 'Viewer' window and let it run until close. Three application
-- callbacks must be provided:
-- onInit: Create application specific context.
-- onEvent: Called at every event generated by the runtime.
-- onExit: Destroy application specific context.
-- The OpenGL context will be alive until the onExit handler is done.
viewScenes :: Configuration
           -> (Viewer -> IO a)
           -> (Viewer -> Event -> a -> IO a)
           -> (Viewer -> a -> IO ())
           -> IO (Either String ())
viewScenes configuration onInit onEvent onExit = do
    result <- makeWindow configuration
    case result of
        -- GL context is created. Start everything up.
        Right (window, width', height') -> do

            -- Create the shared data between the renderer and the application.
            logger <- mkLogger <| debugContext configuration
            currentScene <- newTVarIO $!! initialScene configuration
            renderState <- newTVarIO $!! Initializing
            eventQueue  <- newTQueueIO
            programRequest <- newTQueueIO
            programReply <- newTQueueIO
            meshRequest <- newTQueueIO
            meshReply <- newTQueueIO
            textureRequest <- newTQueueIO
            textureReply <- newTQueueIO

            -- Start the render thread.
            viewport <- newIORef Viewport { width = width', height = height' }
            thread <- asyncBound $
                renderThread (globalSettings configuration)
                    Runtime.Runtime
                        { Runtime.window = window
                        , Runtime.logger = logger
                        , Runtime.viewport = viewport
                        , Runtime.frameStart = 0
                        , Runtime.currentScene = currentScene
                        , Runtime.renderState = renderState
                        , Runtime.eventQueue = eventQueue
                        , Runtime.programRequest = programRequest
                        , Runtime.programReply = programReply
                        , Runtime.meshRequest = meshRequest
                        , Runtime.meshReply = meshReply
                        , Runtime.textureRequest = textureRequest
                        , Runtime.textureReply = textureReply
                        }

            -- Continue with application thread in the current thread.
            applicationThread onInit onEvent onExit
                Viewer.Viewer
                    { Viewer.renderThread = thread
                    , Viewer.logger = logger
                    , Viewer.currentScene = currentScene
                    , Viewer.renderState = renderState
                    , Viewer.eventQueue = eventQueue
                    , Viewer.programRequest = programRequest
                    , Viewer.programReply = programReply
                    , Viewer.meshRequest = meshRequest
                    , Viewer.meshReply = meshReply
                    , Viewer.textureRequest = textureRequest
                    , Viewer.textureReply = textureReply
                    }
            return $ Right ()

        -- Cannot create a GL context.
        Left err                      -> return $ Left err

-- | Entry for the renderering thread. This must run in a bound thread due
-- to OpenGL using thread local storage.
renderThread :: [Setting] -> Runtime -> IO ()
renderThread globalSettings' runtime = do
    -- Make the OpenGL context current for this thread.
    GLFW.makeContextCurrent (Just $ Runtime.window runtime)

    -- Subscribe to mandatory callbacks.
    subscribeToMandatoryCallbacks runtime

    -- Apply global, persistant, settings.
    applyPersistantSettings globalSettings'

    -- Run the render loop until 'RenderState' value Done is reached.
    renderLoop runtime

    -- Terminate OpenGL, and then just return back.
    GLFW.terminate

-- | The render loop, running in the render thread.
renderLoop :: Runtime -> IO ()
renderLoop = go
    where
        go :: Runtime -> IO ()
        go runtime = do
            -- Start by scanning the request queues.
            Runtime.scanRequests runtime

            Just now <- GLFW.getTime
            renderState <- Runtime.getRenderState runtime

            -- If the 'RenderState' is Running, emit a Frame event to the
            -- application.
            viewport <- Runtime.getViewport runtime
            when (renderState == Running) $ do
                let duration = now - Runtime.frameStart runtime
                Runtime.emitEvent runtime $ Frame duration viewport

            -- Render the current scene.
            Scene.render viewport =<< Runtime.getCurrentScene runtime

            let window = Runtime.window runtime

            -- Swap buffers and make the scene visible.
            GLFW.swapBuffers window

            -- Poll event and run callbacks.
            GLFW.pollEvents

            -- Is the window close flag set?
            shallClose <- GLFW.windowShouldClose window
            when shallClose $ do
                -- If so, signal to application with a CloseRequest.
                Runtime.emitEvent runtime CloseRequest
                -- And reset the close flag.
                GLFW.setWindowShouldClose window False

            -- If the 'RenderState' is set to Done we close. Otherwise just
            -- go on.
            unless (renderState == Done) $ go runtime { Runtime.frameStart = now }

-- | Entry for the application thread. When we enter the function the
-- 'RenderState' is Initializing.
applicationThread :: (Viewer -> IO a)
                  -> (Viewer -> Event -> a -> IO a)
                  -> (Viewer -> a -> IO ())
                  -> Viewer
                  -> IO ()
applicationThread onInit onEvent onExit viewer = do
    -- Log startup information.
    logStartUpInfo <| Viewer.logger viewer

    -- Let the application initialize itself.
    infoLog (Viewer.logger viewer) "gl-scene enter onInit."
    app <- onInit viewer
    infoLog (Viewer.logger viewer) "gl-scene exit onInit."

    -- Enter running state, and wait until the application chose to enter
    -- Closing state.
    infoLog (Viewer.logger viewer) "gl-scene enter running state."
    Viewer.setRenderState viewer Running
    app' <- go app
    infoLog (Viewer.logger viewer) "gl-scene exit running state."

    -- Let the application clean up stuff.
    infoLog (Viewer.logger viewer) "gl-scene enter onExit."
    onExit viewer app'
    infoLog (Viewer.logger viewer) "gl-scene exit onExit."

    -- Set the 'RenderState' to Done.
    Viewer.setRenderState viewer Done

    -- And, finally, wait for the render thread to terminate.
    Viewer.waitOnTermination viewer

    -- Tell we are stopping, and then close the logger.
    infoLog (Viewer.logger viewer) "gl-scene stop."
    closeLogger <| Viewer.logger viewer
    where
        go app = do
            -- Wait for the next 'Event.'
            event <- Viewer.getNextEvent viewer

            -- Feed it to the application.
            app' <- onEvent viewer event app

            -- Check 'RenderState'. End this loop if the application
            -- has requested a close.
            renderState <- Viewer.getRenderState viewer
            if renderState /= Closing
                then go app'
                else return app'

-- | Do all the low level stuff to setup a GL context/GLFW window.
makeWindow :: Configuration -> IO (Either String (Window, Int, Int))
makeWindow configuration =
    runExceptT $ do
        initSuccess <- liftIO GLFW.init
        unless initSuccess $ throwError "GLFW initialization failed"

        liftIO (GLFW.windowHint $ WindowHint'Resizable True)
        liftIO (GLFW.windowHint $ WindowHint'Samples 8)
        liftIO (GLFW.windowHint $ WindowHint'ContextVersionMajor (glVersionMajor configuration))
        liftIO (GLFW.windowHint $ WindowHint'ContextVersionMinor (glVersionMinor configuration))
        liftIO (GLFW.windowHint $ WindowHint'OpenGLForwardCompat True)
        liftIO (GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core)
        liftIO (GLFW.windowHint $ WindowHint'OpenGLDebugContext (debugContext configuration))

        case displayMode configuration of
            FullScreen -> do
                monitor' <- liftIO GLFW.getPrimaryMonitor
                when (isNothing monitor') $
                    throwError "Cannot get hold of primary monitor"
                let Just monitor = monitor'

                mode' <- liftIO $ GLFW.getVideoMode monitor
                when (isNothing mode') $
                    throwError "Cannot get hold of monitor's video mode"

                let Just mode = mode'
                    width' = videoModeWidth mode
                    height' = videoModeHeight mode

                win' <- liftIO $
                    GLFW.createWindow width' height' (caption configuration) (Just monitor) Nothing

                when (isNothing win') $
                    throwError "Cannot create fullscreen window"
                let Just win = win'

                return (win, width', height')

            Windowed width' height' -> do
                win' <- liftIO $
                    GLFW.createWindow width' height' (caption configuration) Nothing Nothing
                when (isNothing win') $
                    throwError "Cannot created windowed window"

                let Just win = win'

                return (win, width', height')

logStartUpInfo :: Logger -> IO ()
logStartUpInfo logger = do
    numProcs <- getNumProcessors
    numThreads <- getNumCapabilities
    infoLog logger <| printf "gl-scene start. %d thread(s). %d processor(s)"
                             numThreads numProcs
