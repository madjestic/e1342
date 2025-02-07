{-# LANGUAGE OverloadedStrings #-}

module Main where

import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL

import Graphics.GL
import SDL

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
  
s = 1000000 :: Int
m = 60 * s  :: Int  

data Ticker =
    TString String
  | TInt    Int
  deriving (Show)

drawGUI :: MVar Ticker -> IO ()
drawGUI mvar = do
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

    liftIO $ mainLoop window mvar

mainLoop :: Window -> MVar Ticker -> IO ()
mainLoop window mvar = unlessQuit $ do
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen "Hello, ImGui!" $ do
    t <- readMVar mvar
    -- Add a text widget
    text $ pack $ show t

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button "Clickety Click" >>= \clicked ->
      when clicked $ putStrLn $ "FRP thread says hello!\nvalue : " ++ show t ++ "\n"

  -- Show the ImGui demo window
  showDemoWindow

  -- Render
  glClear GL_COLOR_BUFFER_BIT
  render
  openGL3RenderDrawData =<< getDrawData

  glSwapWindow window
  mainLoop window mvar
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

tickerPlus :: MVar Ticker -> IO ()
tickerPlus imv = do
  threadDelay s
  imv' <- takeMVar imv
  _ <- putMVar imv . TInt $ fromTicker imv' + 1
  tickerPlus imv
    where
      fromTicker :: Ticker -> Int
      fromTicker (TInt v) = v
      fromTicker _ = error "UnsupportedType"

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

animate :: MVar Ticker 
        -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
        -> IO ()
animate mvar sf = do
  reactimateB $ input >>> sfIO >>> output mvar
  quit
  where
    input     = arr (const (0.2, (initSettings, ())))                        :: MSF IO b (DTime, (GameSettings, ()))
    sfIO      = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) initGame :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
    output mvar = arrM (renderOutput mvar)                                        :: MSF IO   (Game, Maybe Bool) Bool

renderOutput :: MVar Ticker -> (Game, Maybe Bool) -> IO Bool
renderOutput mvar (g,q) = do 
  threadDelay s
  imv' <- takeMVar mvar
  _ <- putMVar mvar . TInt $ fromTicker imv' - 1
  return False
    where
      fromTicker :: Ticker -> Int
      fromTicker (TInt v) = v
      fromTicker _ = error "UnsupportedType"

  

tickerfrp :: MVar Ticker -> IO ()
tickerfrp imv = do
  threadDelay s
  imv' <- takeMVar imv
  _ <- putMVar imv . TInt $ fromTicker imv' + 1
  tickerPlus imv
    where
      fromTicker :: Ticker -> Int
      fromTicker (TInt v) = v
      fromTicker _ = error "UnsupportedType"

ticker2 :: MVar Ticker -> IO ()
ticker2 smv = do
  threadDelay $ s `div` 2
  smv' <- takeMVar smv :: IO Ticker
  _ <- putMVar smv . TString $ fromTicker smv' ++ "ticker2"
  ticker2 smv
    where
      fromTicker :: Ticker -> String
      fromTicker (TString v) = v
      fromTicker _ = error "UnsupportedType"

querryTickers :: [MVar Ticker] -> IO [Ticker]
querryTickers mvars = do
  mapM querryTicker  mvars
  where
    querryTicker :: MVar Ticker -> IO Ticker
    querryTicker mv = do
      threadDelay $ s `div` 2
      readMVar mv

printTickers :: [Ticker] -> IO ()
printTickers ts = do
  mapM_ printTicker ts
  where
    printTicker :: Ticker -> IO ()
    printTicker t = do
      print $ toPrintable t
        where
          toPrintable t = case t of
            TString s' -> s'
            TInt    i' -> show i'

main :: IO ()
main = do
  putStrLn "Start"
  tickerMVar  <- newMVar $ TInt 0 :: IO (MVar Ticker)
  _ <- forkIO $ tickerPlus tickerMVar

  ticker2MVar <- newMVar $ TString "null" :: IO (MVar Ticker)
  _ <- forkIO $ ticker2 ticker2MVar

  tickerFRPMVar  <- newMVar $ TInt (-10) :: IO (MVar Ticker)
  _ <- forkIO $ animate tickerFRPMVar game

  _ <- forkIO $ drawGUI tickerFRPMVar

  _ <- forkIO $ forever $ querryTickers [tickerMVar, ticker2MVar] >>= printTickers

  threadDelay 15000000
  putStrLn "End"
  return ()
  
