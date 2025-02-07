{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import SDL
import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL as GL
import Graphics.GL hiding (GLfloat)
import Data.Text (Text, pack)  
import Foreign.Marshal.Array (withArray)
import Foreign.Storable      (sizeOf)
import GHC.Ptr
import Control.Concurrent    (newMVar, swapMVar,forkIO)
import DearImGui             (newFrame
                             ,withWindowOpen
                             ,text
                             ,button
                             ,render
                             ,getDrawData
                             ,createContext
                             ,destroyContext)
import DearImGui.OpenGL3     (openGL3NewFrame
                             ,openGL3RenderDrawData
                             ,openGL3Init
                             ,openGL3Shutdown)
import DearImGui.SDL         (sdl2NewFrame
                             ,pollEventWithImGui
                             ,sdl2Shutdown)
import DearImGui.SDL.OpenGL  (sdl2InitForOpenGL)
import DearImGui.Raw (showDemoWindow)
import Control.Monad.Managed
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Exception (bracket, bracket_)
import Unsafe.Coerce
import Linear.Affine
import Linear.V2  
import Foreign.C.Types
import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.MSF.State  (runStateS_, StateT (..))
import Control.Monad.Trans.MSF.Reader (runReaderS, ReaderT (..))
import Control.Monad.Trans.MSF.Maybe  (runMaybeS)
import Control.Monad.Trans.MSF as TMSF
import Data.MonadicStreamFunction
import Graphics.RedViz.LoadShaders

import Debug.Trace as DT


type DTime = Double

-- < Rendering > -------------------------------------------------------------
openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear                    
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality          
       when (renderQuality /= SDL.ScaleLinear) $                    
         putStrLn "Warning: Linear texture filtering not enabled!"

    let config = OpenGLConfig { glColorPrecision     = V4 8 8 8 0
                              , glDepthPrecision     = 24
                              , glStencilPrecision   = 8
                              , glMultisampleSamples = 4
                              , glProfile            = Core Normal 4 5
                              }
     
    window <- SDL.createWindow
              title
              SDL.defaultWindow
              { SDL.windowInitialSize     = V2 sizex sizey
              , SDL.windowGraphicsContext = OpenGLContext config }
              
    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window

type Drawable   = [Vertex4 Double]
type Pos        = (Double, Double)  
data Shape      = Square Pos Double
                deriving Show

toVertex4 :: Pos -> Vertex4 Double
toVertex4 (k, l)   = Vertex4 k l 0 1

square :: Pos -> Double -> [Pos]
square pos side = [p1, p2, p3,
                   p1, p3, p4]
    where          
        x = fst pos
        y = snd pos
        r = side/2 
        p1 = (x + r, y + r)
        p2 = (x - r, y + r)
        p3 = (x - r, y - r)
        p4 = (x + r, y - r)

toPos :: Shape -> [Pos]
toPos (Square pos side) =  square pos side

data Projection = Planar                
                deriving Show 

type UV         = [TexCoord2 Double] 

toUV :: Projection -> UV
toUV Planar =
  projectPlanar ps
  where
    projectPlanar :: [Pos] -> UV
    projectPlanar = map $ uncurry TexCoord2                                                                   
    ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
         ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)] :: [Pos]

toDrawable :: Shape -> Drawable
toDrawable x = map toVertex4 $ toPos x

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

initResources :: ([Vertex4 Double]) -> Double -> IO Descriptor
initResources (vs) timer = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    --
    -- Declaring VBO: vertices
    --
    let vertices = vs
        numVertices = length vertices

    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 4 GL.Double 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    --
    -- Declaring VBO: UVs
    --
    let uv = toUV Planar

    textureBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just textureBuffer
    withArray uv $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head uv))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let uvCoords = AttribLocation 1
    vertexAttribPointer uvCoords $=
        (ToFloat, VertexArrayDescriptor 2 GL.Double 0 (bufferOffset firstIndex))
    vertexAttribArray uvCoords   $= Enabled

    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "shaders/shader.frag")]
    currentProgram $= Just program

    -- Set Uniforms
    location <- GL.get (uniformLocation program "fTime")
    uniform location $= (realToFrac timer :: GLfloat)

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)    

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
  
renderOutput :: (Window, GLContext) -> (Game, Maybe Bool) -> IO Bool
renderOutput _ ( _,Nothing) = quit >> return True
renderOutput (window,_) (g1,_) = do
  let
    drawable = toDrawable (Square (0.0, 0.0) 1.0)
    timer    = 0.01 * (fromIntegral $ tick g1)
  (Descriptor triangles firstIndex numVertices) <- initResources drawable timer
  
  GL.clearColor $= Color4 0 0 0 1
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices

  SDL.glSwapWindow window >> return False


  -- < GUI > --------------------------------------------------------------------
drawGUI :: Window -> (Game, Maybe Bool) -> IO ()
drawGUI window _ = unlessQuit do
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- -- Build the GUI
  -- withWindowOpen (pack "Hello, ImGui!") do
  --   -- Add a text widget
  --   text (pack "Hello, ImGui!")

  --   -- Add a button widget, and call 'putStrLn' when it's clicked
  --   button (pack "Clickety Click") >>= \case
  --     False -> return ()
  --     True  -> putStrLn "Ow!"

  -- Show the ImGui demo window
  showDemoWindow

  -- Render
  -- let p0 = (0,0) :: (Double, Double)
  --     z0 = 0     :: Double
  -- (Descriptor triangles numIndices) <- initResources (verts p0) indices z0

  -- GL.clearColor $= Color4 0 0 0 1
  -- GL.clear [ColorBuffer]

  -- bindVertexArrayObject $= Just triangles
  -- drawElements Triangles numIndices GL.UnsignedInt nullPtr

  render
  -- openGL3RenderDrawData =<< getDrawData

  SDL.glSwapWindow window

  --mainLoop window

  where
    -- Process the event loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action

    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event = SDL.eventPayload event == SDL.QuitEvent
-- < Game Loop > --------------------------------------------------------------
game :: MSF (MaybeT (ReaderT GameSettings (ReaderT Double (StateT Game IO)))) () Bool
game = gameLoop `untilMaybe` gameQuit `catchMaybe` exit
  where
    gameLoop = arrM (\_ -> (lift . lift . lift) gameLoop')
    gameQuit = arrM (\_ -> (lift . lift . lift) gameQuit')

    gameQuit' :: StateT Game IO Bool
    gameQuit' = TMSF.get >>= \s -> return $ quitGame s

    gameLoop' :: StateT Game IO Bool
    gameLoop' = do
      handleEvents
        where
          handleEvents :: StateT Game IO Bool
          handleEvents = do
            liftIO $ delay 10
            events <- SDL.pollEvents
            updateKeyboard mapKeyEvents events
            updateMouse events
            let result = any isQuit $ fmap eventPayload events :: Bool
            --get >>= (liftIO . print)
            return result
              where
                isQuit :: EventPayload -> Bool
                isQuit ev =
                  case ev of
                    KeyboardEvent keyboardEvent -> 
                      keyboardEventKeyMotion keyboardEvent                  == Pressed
                      && keysymScancode (keyboardEventKeysym keyboardEvent) == ScancodeQ
                    QuitEvent -> True
                    _         -> False
                
                mapKeyEvents :: [(Scancode, StateT Game IO ())]
                mapKeyEvents =
                  [ (ScancodeW, inc   10)
                  , (ScancodeS, inc (-10))
                  , (ScancodeQ, exit' True) ]
                  where
                    inc :: Integer -> StateT Game IO ()
                    inc n = modify $ inc' n
                      where
                        inc' :: Integer -> Game -> Game
                        inc' k (Game c m q) =
                          Game
                          { tick      = c + k
                          , mpos      = m
                          , quitGame  = q
                          }
                     
                    exit' :: Bool -> StateT Game IO ()
                    exit' b = modify $ quit' b
                     
                    quit' :: Bool -> Game -> Game
                    quit' b gameLoop' = gameLoop' { quitGame = b }

                updateMouse  :: [Event] -> StateT Game IO ()
                updateMouse = mapM_ processEvent 
                  where
                    processEvent :: Event -> StateT Game IO ()
                    processEvent e =
                      let mk = case eventPayload e of
                            MouseMotionEvent mouseEvent -> Just (mouseMotionEventPos mouseEvent)
                            _ -> Nothing
                      in case mk of
                        Nothing   -> return ()
                        Just vpos -> mmove (unsafeCoerce vpos)
                                     where
                                       mmove :: Point V2 CInt -> StateT Game IO ()
                                       mmove pos = modify $ mmove' pos
                                         where
                                           mmove' :: Point V2 CInt -> Game -> Game
                                           mmove' pos (Game c m q) =
                                             Game
                                             { tick     = c
                                             , mpos     = pos
                                             , quitGame = q }
  
                updateKeyboard :: (Monad m) => [(Scancode, m ())] -> [Event] -> m ()
                updateKeyboard ns = mapM_ (processEvent ns)
                  where
                    processEvent :: (Monad m) => [(Scancode , m ())] -> Event -> m ()
                    processEvent mapping e =
                      let mk = case eventPayload e of
                                 KeyboardEvent keyboardEvent -> Just
                                   ( keyboardEventKeyMotion keyboardEvent == Pressed
                                   , keysymScancode (keyboardEventKeysym keyboardEvent))
                                 _ -> Nothing
                      in case mk of
                        Nothing     -> return ()
                        Just (_, k) -> case lookup k mapping of
                                          Nothing -> return ()
                                          Just k  -> k

-- < Game Definition > --------------------------------------------------------
data Game = Game
  { tick     :: Integer
  , mpos     :: Point V2 CInt
  , quitGame :: Bool
  } deriving Show

data GameSettings = GameSettings
  { resX :: Int 
  , resY :: Int 
  } deriving Show

initGame :: Game
initGame =
  Game
  { tick     = -1
  , mpos     = P (V2 0 0)
  , quitGame = False
  }

initSettings :: GameSettings
initSettings = GameSettings
  {
    resX = 800
  , resY = 600
  }

-- animate :: (Window, GLContext)
--          -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
--          -> IO ()
-- animate (window,glContext) sf = do
--   --renderer <- createRenderer window (-1) defaultRenderer
--   reactimateB $ input >>> sfIO >>> output (window,glContext)
--   quit
--   where
--     input    = arr (const (0.2, (initSettings, ())))                        :: MSF IO b (DTime, (GameSettings, ()))
--     sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) initGame :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
--     output (w,g) = arrM (renderOutput (w,g))                                  :: MSF IO   (Game, Maybe Bool) Bool

-- animate :: (Window, GLContext)
-- --         -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
--          -> IO ()
-- animate (window,glContext) = do
--   mainLoop window (initGame, Nothing)
--   -- runManaged do
--   --   -- Create an ImGui context
--   --   _ <- managed $ bracket createContext destroyContext
--   --   -- Initialize ImGui's SDL2 backend
--   --   _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
--   --   -- Initialize ImGui's OpenGL backend
--   --   _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown
      
--     --liftIO $ reactimateB $ input >>> sfIO >>> output (window,glContext)
--   --output' (window,glContext)
--   -- quit
--     --liftIO $ reactimate input :: _  --input :: _ --  >>> sfIO >> output (window,glContext)
--                -- output
--                -- sf
--     --quit
--     where
--       input    = arr (const (0.2, (initSettings, ())))
--       sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) initGame
--       output (w,g) = arrM (renderOutput (w,g))
--       output' (w,g) = mainLoop w --arrM (mainLoop w >> (\_ -> return False))
--       --output (w,g) = undefined --arrM (mainLoop w >> (\_ -> return False))

-- WORKING --
-- animate :: (Window, GLContext)
--          -> IO ()
-- animate (window,glContext)= do
--   runManaged do
--     -- Create an ImGui context
--     _ <- managed $ bracket createContext destroyContext
--     -- Initialize ImGui's SDL2 backend
--     _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
--     -- Initialize ImGui's OpenGL backend
--     _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

--     liftIO $ mainLoop window (initGame, Nothing)


animate :: (Window, GLContext)
         -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
         -> IO ()
animate (window,glContext) sf = do
  runManaged do
    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext
    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

    --liftIO $ reactimateB $ input >>> sfIO >>> output (window,glContext)
    --liftIO $ reactimateB $ input >>> sfIO >>> output'' window
    --liftIO $ mainLoop' (window, glContext) (initGame, Nothing)
    --liftIO $ reactimateB $ input >>> sfIO >>> output''' window
    liftIO $ output' window
    --quit
      where
        --input    = arr (const (0.2, (initSettings, ())))
        sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) initGame
        output (w,g) = arrM (renderOutput (w,g))
        output' w = mainLoop window (initGame, Nothing)
        --output'' w = arrM (mainLoop window  >> (\_ -> return False))
        output'' w = arrM (mainLoopWrapper (window, glContext)  >> (\_ -> return False))
        output''' w = arr $ mainLoop (window) >> (\_ -> return False)


animate' :: (Window, GLContext)
         -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
         -> IO ()
animate' (window,glContext) sf = do
  runManaged do
    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext
    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

    liftIO $ reactimateB $ input >>> sfIO >>> output' (window,glContext)

    --liftIO $ output (window,glContext)
      where
        input    = arr (const (0.2, (initSettings, ())))
        sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) initGame
        output' (w,g) = arrM (renderOutput (w,g))                             
        --output (w,g) = (mainLoop w (initGame, Nothing))
    -- liftIO $ reactimateB $ input >>> sfIO >>> output (window,glContext)
  -- quit
  -- where
  --   input    = arr (const (0.2, (initSettings, ())))                        :: MSF IO b (DTime, (GameSettings, ()))
  --   sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) initGame :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
  --   --output (w,g) = arrM (renderOutput (w,g))                                :: MSF IO   (Game, Maybe Bool) Bool
  --   output (w,g) = arrM (mainLoop w >> (\_ -> return False))              :: MSF IO   (Game, Maybe Bool) Bool


-- foo :: (Game, Maybe Bool) -> IO Bool
-- foo _ = do
--   print $ "Pong!"
--   return False

-- main :: IO ()
-- main = do
--   let (resX', resY') =
--         (\opts ->
--            ( unsafeCoerce $ fromIntegral $ resX opts
--            , unsafeCoerce $ fromIntegral $ resY opts))
--         initSettings
--   initializeAll
--   window <- openWindow "Mandelbrot + SDL2 + OpenGL + dear-imgui" (resX', resY')

--   _ <- setMouseLocationMode RelativeLocation
--   _ <- warpMouse (WarpInWindow window) (P (V2 resX' resY'))
--   _ <- cursorVisible $= True

--   animate window game
--   putStrLn "Exiting Game"

-- main :: IO ()
-- main = do
--   let (resX', resY') =
--         (\opts ->
--            ( unsafeCoerce $ fromIntegral $ resX opts
--            , unsafeCoerce $ fromIntegral $ resY opts))
--         initSettings
--   -- Initialize SDL
--   initializeAll

--   runManaged do
--     -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
--     window <- do
--       let title  = "Hello, Dear ImGui!"
--       let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL
--                                  , windowInitialSize     = V2 resX' resY'}
--       managed $ bracket (createWindow (pack title) config) destroyWindow

--     -- Create an OpenGL context
--     glContext <- managed $ bracket (glCreateContext window) glDeleteContext

--     --liftIO $ animate glContext window (parseWinInput >>> ( ((game >>^ zoom) &&& (game >>^ pos)) &&& handleExit))
--     _ <- setMouseLocationMode RelativeLocation
--     _ <- warpMouse (WarpInWindow window) (P (V2 resX' resY'))
--     _ <- cursorVisible $= True

--     liftIO $ animate (window, glContext) game

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll

  runManaged $ do
    -- Create a window using SDL; as we're using OpenGL, we enable OpenGL too
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
      managed $ bracket (createWindow (pack title) config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext
    -- -- Create an ImGui context
    -- _ <- managed $ bracket createContext destroyContext

    -- -- Initialize ImGui's SDL2 backend
    -- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    -- -- Initialize ImGui's OpenGL backend
    -- managed_ $ bracket_ openGL3Init openGL3Shutdown

    --liftIO $ mainLoop window (initGame,Nothing)
    liftIO $ animate (window, glContext) game

mainLoopWrapper :: (Window, GLContext) -> (Game, Maybe Bool) -> IO ()  
mainLoopWrapper (window, glContext) _ = do
  runManaged do
    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext
    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

    liftIO $ mainLoop window (initGame, Nothing)

mainLoopWrapper' :: (Window, GLContext) -> (Game, Maybe Bool) -> IO ()  
mainLoopWrapper' (window, glContext) _ = do
  runManaged do
    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext
    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

    liftIO $ mainLoop window (initGame, Nothing)

mainLoop :: Window -> (Game, Maybe Bool) -> IO ()
--mainLoop :: Window -> IO ()
mainLoop window (g,mq) = unlessQuit $ do
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen "Hello, ImGui!" $ do
    -- Add a text widget
    text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button "Clickety Click" >>= \clicked ->
      when clicked $ putStrLn "Ow!"

  -- Show the ImGui demo window
  -- showDemoWindow

  -- Render
  glClear GL_COLOR_BUFFER_BIT
  render
  openGL3RenderDrawData =<< getDrawData

  glSwapWindow window
  mainLoop window (g,mq)
  where
  -- Process the event loop
  unlessQuit action = do
    shouldQuit <- gotQuitEvent
    unless shouldQuit action

  gotQuitEvent = do
    ev <- pollEventWithImGui

    case ev of
      Nothing ->
        return False
      Just event ->
        (isQuit event ||) <$> gotQuitEvent

  isQuit event =
    eventPayload event == QuitEvent

mainLoop' :: (Window, GLContext) -> (Game, Maybe Bool) -> IO ()
--mainLoop :: Window -> IO ()
mainLoop' (window,glc) (g,mq) = unlessQuit $ do
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen "Hello, ImGui!" $ do
    -- Add a text widget
    text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button "Clickety Click" >>= \clicked ->
      when clicked $ putStrLn "Ow!"

  -- Show the ImGui demo window
  -- showDemoWindow

  -- Render
  glClear GL_COLOR_BUFFER_BIT
  render
  openGL3RenderDrawData =<< getDrawData

  glSwapWindow window
  mainLoop' (window,glc) (g,mq)
  where
  -- Process the event loop
  unlessQuit action = do
    shouldQuit <- gotQuitEvent
    unless shouldQuit action

  gotQuitEvent = do
    ev <- pollEventWithImGui

    case ev of
      Nothing ->
        return False
      Just event ->
        (isQuit event ||) <$> gotQuitEvent

  isQuit event =
    eventPayload event == QuitEvent
