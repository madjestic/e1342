{-# LANGUAGE OverloadedStrings #-}

module Main where

import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL

import Graphics.GL
import SDL hiding (get)

import Control.Monad.Managed
import Control.Monad.IO.Class ()
import Control.Monad (when, unless, forever)
import Control.Monad.Trans.Class
import Control.Exception (bracket, bracket_)
import Control.Concurrent
import Data.Text (pack)

import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.MSF as TMSF
import Data.MonadicStreamFunction

import Foreign.C.Types
import Unsafe.Coerce
  
data AppInput = AppInput
  { events' :: [Event] }
  deriving Show

s = 1000000 :: Int
m = 60 * s  :: Int  

data Ticker =
    TString String
  | TInt    Int
  deriving (Show)

drawGUI :: MVar AppInput -> MVar Game -> IO ()
drawGUI imvar gmvar = do
  -- Initialize SDL
  initializeAll

  runManaged $ do
    -- Create a window using SDL; as we're using OpenGL, we enable OpenGL too
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext
    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    -- Initialize ImGui's OpenGL backend
    managed_ $ bracket_ openGL3Init openGL3Shutdown

    liftIO $ guiLoop window imvar gmvar

guiLoop :: Window -> MVar AppInput -> MVar Game -> IO ()
guiLoop window imvar gmvar = unlessQuit $ do
  delay 10
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen "Hello, ImGui!" $ do
    g <- readMVar gmvar
    -- Add a text widget
    text . pack . show $ tick g

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button "Clickety Click" >>= \clicked ->
      when clicked $ putStrLn $ "FRP thread says hello!\nvalue : " ++ show g ++ "\n"

  -- Show the ImGui demo window
  showDemoWindow

  -- Render
  glClear GL_COLOR_BUFFER_BIT
  render
  openGL3RenderDrawData =<< getDrawData

  glSwapWindow window
  guiLoop window imvar gmvar
  where
  -- Process the event loop
  unlessQuit action = do
    shouldQuit <- gotQuitEvent
    unless shouldQuit action
    where
      gotQuitEvent = do
        --events <- pollEventsWithImGui
        -- event <- pollEventWithImGui
        -- let event = Nothing
        -- sample event loop and write it to share AppInput MVar
        -- input <- takeMVar imvar
        --_ <- putMVar imvar $ input { events' = events}
        return False
        -- case event of
        --   Nothing -> do
        --     _ <- putMVar imvar $ input { events' = []}
        --     return False
        --   Just event -> do
        --     _ <- putMVar imvar $ input { events' = []}
        --     (isQuit event ||) <$> gotQuitEvent
        -- where
        --   isQuit event =
        --     eventPayload event == QuitEvent

-- < Game Definition > --------------------------------------------------------
type DTime = Double

data Game = Game
  { tick     :: Integer
  , mpos     :: Point V2 CInt
  , quitGame :: Bool
  } deriving Show

data GameSettings = GameSettings
  { resX :: Int 
  , resY :: Int 
  } deriving Show

initAppInput :: AppInput
initAppInput =
  AppInput
  {
    events' = []
  }

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

-- | MSF-based timer that increments every second
timerMSF :: MonadIO m => MSF m () Double
timerMSF = feedback 0 $ arrM $ \((), t) -> do
    liftIO $ threadDelay 1000000  -- Wait for 1 second
    return (t + 1, t + 1)

gameSF :: MVar AppInput -> MVar Game -> MSF (MaybeT (ReaderT GameSettings (ReaderT Double (StateT Game IO)))) () Bool
gameSF imvar gmvar = gameLoop `untilMaybe` gameQuit `catchMaybe` exit
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
            -- events <- SDL.pollEvents
            events <- pollEventsWithImGui
            -- input <- liftIO $ readMVar imvar 
            -- let events = events' input
            updateKeyboard mapKeyEvents events
            updateMouse events
            let result = any isQuit $ fmap eventPayload events :: Bool
            --get >>= (liftIO . print)
            _ <- liftIO $ takeMVar gmvar
            get >>= liftIO . putMVar gmvar
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

animate :: MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
        -> IO ()
animate sf = do
  reactimateB $ env >>> sfIO >>> arrM (\_ -> return False)
  quit
  where
    env  = arr (const (0.2, (initSettings, ())))                        :: MSF IO b (DTime, (GameSettings, ()))
    sfIO = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) initGame :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)

main :: IO ()
main = do
  putStrLn "Start"
  
  imvar <- newMVar $ initAppInput :: IO (MVar AppInput)
  gmvar <- newMVar $ initGame :: IO (MVar Game)
  _ <- forkIO $ animate (gameSF imvar gmvar)
  _ <- forkIO $ drawGUI imvar gmvar

  threadDelay 15000000
  putStrLn "End"
  return ()
  
