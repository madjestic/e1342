{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Data.Text     (Text
                     ,pack )
import Foreign.C     (CInt)
import Control.Monad (when)
import Graphics.Rendering.OpenGL as GL (depthFunc
                                       ,ComparisonFunction( Less )
                                       ,VertexArrayObject
                                       ,NumArrayIndices
                                       ,clearColor
                                       ,clear
                                       ,DataType(..)
                                       ,($=)
                                       ,GLfloat
                                       ,GLuint
                                       ,GLsizei
                                       ,GLmatrix
                                       ,newMatrix
                                       ,genObjectName
                                       ,bindVertexArrayObject
                                       ,bindBuffer
                                       ,BufferTarget (..)
                                       ,BufferUsage (..)
                                       ,bufferData
                                       ,AttribLocation (..)
                                       ,vertexAttribPointer
                                       ,IntegerHandling (..)
                                       ,VertexArrayDescriptor (..)
                                       ,vertexAttribArray
                                       ,Capability (..)
                                       ,ShaderType (..)
                                       ,currentProgram
                                       ,uniformLocation
                                       ,uniform
                                       ,MatrixOrder (..)
                                       ,Color4 (..)
                                       ,ClearBuffer (..)
                                       ,drawElements
                                       ,PrimitiveMode (..))
import SDL                      (OpenGLConfig (..)
                                ,WindowGraphicsContext (..)
                                ,RenderScaleQuality( ScaleLinear )
                                ,Window (..)
                                ,InitFlag (..)
                                ,Hint (..)
                                ,initialize
                                ,get
                                ,createWindow
                                ,defaultWindow
                                ,windowInitialSize
                                ,windowGraphicsContext
                                ,showWindow
                                ,glCreateContext
                                ,destroyWindow
                                ,quit
                                ,V4 (..)
                                ,V2 (..)
                                ,Profile (..)
                                ,Mode (..)
                                ,glSwapWindow
                                ,eventPayload
                                ,EventPayload (..)
                                ,GLContext (..)
                                ,time
                                ,pollEvent
                                ,Point (..)
                                ,keyboardEventKeyMotion
                                ,mouseButtonEventMotion
                                ,mouseButtonEventButton
                                ,mouseMotionEventPos
                                ,InputMotion (..)
                                ,MouseButton (..)
                                ,KeyboardEventData (..)
                                ,keysymScancode
                                ,keyboardEventKeysym
                                ,initializeAll
                                ,defaultOpenGL
                                ,glCreateContext
                                ,glDeleteContext
                                )
import SDL.Input.Keyboard.Codes
import Foreign.Marshal.Array (withArray)
import Foreign.Storable      (sizeOf)
import GHC.Ptr
import Control.Concurrent    (newMVar, swapMVar)
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
import FRP.Yampa
import Control.Monad.Managed
import Control.Exception (bracket
                         ,bracket_)

import Graphics.RedViz.LoadShaders

import Debug.Trace as DT

  
data Descriptor =
     Descriptor VertexArrayObject NumArrayIndices

initResources :: [GLfloat] -> [GLuint] -> Double -> IO Descriptor
initResources vs idx z0 =  
  do
    -- | VAO
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length vs
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length indices
    withArray idx $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * (length indices))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 8 * floatSize

    -- | Positions
    let vPosition  = AttribLocation 0
        posOffset  = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | UV
    let uvCoords   = AttribLocation 1
        uvOffset   = 6 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled

    -- || Shaders
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource "shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "shaders/shader.frag")]
    currentProgram $= Just program

    -- || Set Uniforms
    location <- get (uniformLocation program "fTime")
    uniform location $= (realToFrac z0 :: GLfloat)

    -- || Set Transform Matrix
    let tr :: [GLfloat]
        tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 1 ]
          
    transform <- GL.newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location2 <- get (uniformLocation program "transform")
    uniform location2 $= (transform)

    -- || Unload buffers
    bindVertexArrayObject         $= Nothing

    return $ Descriptor triangles (fromIntegral numIndices)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) =
  do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality
       when (renderQuality /= SDL.ScaleLinear) $
         putStrLn "Warning: Linear texture filtering not enabled!"

    let config = OpenGLConfig { glColorPrecision = V4 8 8 8 0
                              , glDepthPrecision = 24
                              , glStencilPrecision = 8
                              , glMultisampleSamples = 4
                              , glProfile = Core Normal 4 5
                              }

    depthFunc $= Just Less

    window <- SDL.createWindow
              title
              SDL.defaultWindow
              { SDL.windowInitialSize = V2 sizex sizey
              , SDL.windowGraphicsContext = OpenGLContext config
              }      

    SDL.showWindow window
    _ <- SDL.glCreateContext window

    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window = do
    SDL.destroyWindow window
    SDL.quit

indices :: [GLuint]
indices =
  [          -- Note that we start from 0!
    0, 1, 3, -- First Triangle
    1, 2, 3  -- Second Triangle
  ]

verts :: (Double, Double) -> [GLfloat]
verts p0 =
  [ -- | positions    -- | colors      -- | uv
    1.0,  1.0, 0.0,   1.0, 0.0, 0.0,   1.0 + tx, 1.0 + ty,
    1.0, -1.0, 0.0,   0.0, 1.0, 0.0,   1.0 + tx, 0.0 + ty,
   -1.0, -1.0, 0.0,   0.0, 0.0, 1.0,   0.0 + tx, 0.0 + ty,
   -1.0,  1.0, 0.0,   0.0, 0.0, 0.0,   0.0 + tx, 1.0 + ty
  ]
  where
    tx = (\ (x,y)-> realToFrac x) p0 :: GLfloat
    ty = (\ (x,y)-> realToFrac y) p0 :: GLfloat

draw :: SDL.Window -> Double -> (Double, Double) -> IO ()
draw window z0 p0 = do
      (Descriptor triangles numIndices) <- initResources (verts p0) indices z0

      clearColor $= Color4 0 0 0 1
      clear [ColorBuffer]
      bindVertexArrayObject $= Just triangles
      drawElements Triangles numIndices GL.UnsignedInt nullPtr

      --SDL.glSwapWindow window

mainLoop :: Window -> IO ()
mainLoop window = unlessQuit do
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen (pack "Hello, ImGui!") do
    -- Add a text widget
    text . pack $ "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button (pack "Clickety Click") >>= \case
      False -> return ()
      True  -> putStrLn "Ow!"

  -- Show the ImGui demo window
  -- showDemoWindow

  -- Render
  let p0 = (0,0) :: (Double, Double)
      z0 = 0     :: Double
  (Descriptor triangles numIndices) <- initResources (verts p0) indices z0

  GL.clearColor $= Color4 0 0 0 1
  GL.clear [ColorBuffer]

  -- bindVertexArrayObject $= Just triangles
  -- drawElements Triangles numIndices GL.UnsignedInt nullPtr

  render
  openGL3RenderDrawData =<< getDrawData

  SDL.glSwapWindow window

  mainLoop window

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

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent

quitEvent :: SF AppInput (Event ())
quitEvent = arr inpQuit >>> edge
-- | Exported as abstract type. Fields are accessed with signal functions.
-- | AppInput ~= AppInput
data AppInput =
     AppInput
     { inpMousePos    :: (Double, Double)       -- ^ Current mouse position
     , inpMouseLeft   :: Maybe (Double, Double) -- ^ Down button currently down
     , inpMouseRight  :: Maybe (Double, Double) -- ^ Right button currently down
     , inpQuit        :: Bool                   -- ^ SDL's QuitEvent
     , inpKeyPressed  :: Maybe Scancode
     , inpKeyReleased :: Maybe Scancode
     }

updateZoom :: Double -> SF AppInput Double
updateZoom z0 =
  switch sf cont
    where
      sf = proc input -> do
        keyQ    <- key (ScancodeQ) "Pressed" -< input
        keyE    <- key (ScancodeE) "Pressed" -< input
        let res :: (Double, Event (), Event ())
            res = (z0, keyQ  , keyE  )
        returnA -< (z0, (lMerge keyQ   keyE  ) `tag` res)
      cont (x,phse, phle) = if | isEvent phse -> zoomIn  (x)
                               | otherwise    -> zoomOut (x)

key :: Scancode -> String -> SF AppInput (Event ())
key code mode
  | code == ScancodeQ ||
    code == ScancodeE ||
    code == ScancodeA ||
    code == ScancodeD ||
    code == ScancodeW ||
    code == ScancodeS ||        
    code == ScancodeSpace
    = (inpKeyMode ^>> edgeJust) >>^ filterE (code ==) >>^ tagWith ()
      where
       inpKeyMode
         = if | mode == "Pressed"
                -> inpKeyPressed
              | otherwise
                -> inpKeyReleased

zoomIn :: Double -> SF AppInput Double
zoomIn z0 =
  switch sf cont
    where
         sf = proc input -> do
            zoom    <- DT.trace ("z0: " ++ show z0 ++ "\n") $
                       (z0 +) ^<< integral <<< constant 0.1 -< ()
            keyQ    <- key ScancodeQ "Released" -< input
            keyE    <- key ScancodeE "Released" -< input
            returnA -< (zoom, (lMerge keyQ keyE) `tag` zoom) :: (Double, Event Double)
         cont x = updateZoom (x)

zoomOut :: Double -> SF AppInput Double
zoomOut z0 =
  switch sf cont
    where
         sf = proc input -> do
            zoom    <- DT.trace ("z0: " ++ show z0 ++ "\n") $
                       (z0 -) ^<< integral <<< constant 0.1 -< ()
            keyQ    <- key ScancodeQ "Released" -< input
            keyE    <- key ScancodeE "Released" -< input
            returnA -< (zoom, (lMerge keyQ keyE) `tag` zoom) :: (Double, Event Double)            
         cont x = updateZoom (x)

instance (Num a,Num b) => Num (a, b) where
  (+)   (a, b) (c, d) = (a+c, b+d)

updatePos :: (Double, Double) -> SF AppInput (Double, Double)
updatePos p0 =
  switch sf cont
    where
      sf = proc input -> do
        keyA    <- key ScancodeA "Pressed" -< input
        keyD    <- key ScancodeD "Pressed" -< input
        keyW    <- key ScancodeW "Pressed" -< input
        keyS    <- key ScancodeS "Pressed" -< input
        let res :: ((Double, Double)
                   , Event ()
                   , Event ()
                   , Event ()
                   , Event ())
            res =  ( p0
                   , keyA
                   , keyD
                   , keyW
                   , keyS)
        returnA -< (p0, (mergeEvents [ keyA  
                                     , keyD  
                                     , keyW
                                     , keyS ]) `tag` res)

      cont (x, keyA, keyD, keyW, keyS) =
        if | isEvent keyA -> movePos (x) (-0.1, 0.0)
           | isEvent keyD -> movePos (x) ( 0.1, 0.0)
           | isEvent keyW -> movePos (x) ( 0.0, 0.1)  
           | otherwise    -> movePos (x) ( 0.0,-0.1)

movePos :: (Double, Double) -> (Double, Double) -> SF AppInput (Double, Double)
movePos p0 v0 =
  switch sf cont
    where
         sf = proc input -> do
            p       <- DT.trace ("p0: " ++ show p0 ++ "\n") $
                       (p0 +) ^<< integral -< v0
            keyA    <- key ScancodeA "Released" -< input
            keyD    <- key ScancodeD "Released" -< input
            keyW    <- key ScancodeW "Released" -< input
            keyS    <- key ScancodeS "Released" -< input
            returnA -< (p, (mergeEvents
                            [ keyA  
                            , keyD  
                            , keyW
                            , keyS ]) `tag` p) :: ((Double, Double), Event (Double, Double))
         cont x = updatePos (x)

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

initAppInput :: AppInput
initAppInput =
     AppInput
     { inpMousePos    = (0, 0)
     , inpMouseLeft   = Nothing
     , inpMouseRight  = Nothing
     , inpQuit        = False
     , inpKeyPressed  = Nothing
     , inpKeyReleased = Nothing
     }

parseWinInput :: SF WinInput AppInput
parseWinInput = accumHoldBy nextAppInput initAppInput

scancode :: KeyboardEventData -> Scancode
scancode ev =
  keysymScancode $ keyboardEventKeysym ev

nextAppInput :: AppInput -> EventPayload -> AppInput
nextAppInput inp QuitEvent
  = inp { inpQuit = True }
nextAppInput inp (MouseMotionEvent ev) =
    inp { inpMousePos = (fromIntegral x, fromIntegral y) }
    where P (V2 x y) = mouseMotionEventPos ev
nextAppInput inp (KeyboardEvent ev)
    | scancode ev == ScancodeEscape
      = inp { inpQuit = True }
    | scancode ev == ScancodeQ || 
      scancode ev == ScancodeE ||
      scancode ev == ScancodeA ||
      scancode ev == ScancodeD ||
      scancode ev == ScancodeW ||
      scancode ev == ScancodeS ||
      scancode ev == ScancodeSpace
      = if | SDL.keyboardEventKeyMotion ev == Pressed
           -> inp { inpKeyPressed  = Just $ keysymScancode $ keyboardEventKeysym ev
                  , inpKeyReleased = Nothing }
           | otherwise
           -> inp { inpKeyPressed  = Nothing
                  , inpKeyReleased = Just $ keysymScancode $ keyboardEventKeysym ev }
nextAppInput inp (SDL.MouseButtonEvent ev) = inp { inpMouseLeft  = lmb
                                                 , inpMouseRight = rmb }
    where motion = mouseButtonEventMotion ev
          button = mouseButtonEventButton ev
          pos    = inpMousePos inp
          inpMod = case (motion,button) of
              (Released, ButtonLeft)  -> first (const Nothing)
              (Pressed,  ButtonLeft)  -> first (const (Just pos))
              (Released, ButtonRight) -> second (const Nothing)
              (Pressed,  ButtonRight) -> second (const (Just pos))
              _                                      -> id
          (lmb,rmb) = inpMod $ (inpMouseLeft &&& inpMouseRight) inp

nextAppInput inp _ = inp

-- < Game Types > --------------------------------------------------------------
data Game       = Game { zoom :: Double
                       , pos  :: (Double, Double) }
                deriving Show

-- < Game Logic > ---------------------------------------------------------
z0 :: Double
z0 = -3.9053007385999066

p0 :: (Double, Double)
p0 = (-0.15,-0.1)

v0 :: (Double, Double)
v0 = (0,0)

-- < Game > ----------------------------------------------------------------
game :: SF AppInput Game
game = switch sf (\_ -> game)        
     where sf =
             proc input -> do
               gameState <- gameSession  -< input
               reset     <- key (ScancodeSpace) "Pressed" -< input
               returnA   -< (gameState, reset)

gameSession :: SF AppInput Game
gameSession =
  proc input -> do
     zoom <- updateZoom z0 -< input
     pos  <- updatePos  p0 -< input
     returnA -< Game zoom pos

 -- < Animate > ------------------------------------------------------------
type WinInput = Event SDL.EventPayload
type WinOutput = ((Double, (Double, Double)), Bool)

animate :: GLContext
        -> SDL.Window
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate glContext window sf = do
  runManaged do
    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext
    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown
    -- Reactimate -----------------------------------------------------
    liftIO $ reactimate (return NoEvent) -- initialize
               senseInput   
               renderOutput
               sf

    --window <- openWindow title (winWidth, winHeight)
    liftIO $ closeWindow window

      where
    -- Input Logic -----------------------------------------------------
        senseInput _ = do
          lastInteraction <- newMVar =<< SDL.time
          currentTime <- SDL.time                          
          dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
          if False
            then do
              mEvent <- pollEvent                          
              return (dt, Event . SDL.eventPayload <$> mEvent)
            else do
              return (dt, Nothing)
    -- Output Logic -----------------------------------------------------
        renderOutput _ ((zoom, pos), shouldExit) = do
          draw    window zoom pos
          drawUI' window
          SDL.glSwapWindow window
          return shouldExit 

drawUI' :: Window -> IO ()
drawUI' window = unlessQuit do
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen (pack "Hello, ImGui!") do
    -- Add a text widget
    text (pack "Hello, ImGui!")

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button (pack "Clickety Click") >>= \case
      False -> return ()
      True  -> putStrLn "Ow!"

  -- Show the ImGui demo window
  -- showDemoWindow

  -- Render
  -- let p0 = (0,0) :: (Double, Double)
  --     z0 = 0     :: Double
  -- (Descriptor triangles numIndices) <- initResources (verts p0) indices z0

  -- GL.clearColor $= Color4 0 0 0 1
  -- GL.clear [ColorBuffer]

  -- bindVertexArrayObject $= Just triangles
  -- drawElements Triangles numIndices GL.UnsignedInt nullPtr

  render
  openGL3RenderDrawData =<< getDrawData

  --SDL.glSwapWindow window

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

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll

  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
      managed $ bracket (createWindow (pack title) config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext

    liftIO $ animate glContext window (parseWinInput >>> ( ((game >>^ zoom) &&& (game >>^ pos)) &&& handleExit))
