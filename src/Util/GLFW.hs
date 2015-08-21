module Util.GLFW where

import           Control.Concurrent.STM     (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import           Control.Monad
import           Control.Monad.RWS.Strict   (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import           Control.Monad.Trans.Maybe  (MaybeT(..), runMaybeT)
import           Data.Maybe                 (catMaybes)
import qualified Graphics.UI.GLFW as GLFW
import           System.Exit
import           System.IO
import           Util.State

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

joysticks :: [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO GLFW.Window
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    successfulInit <- GLFW.init
    if not successfulInit then exitFailure else do
        GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
        GLFW.windowHint $ GLFW.WindowHint'Decorated False
        GLFW.windowHint $ GLFW.WindowHint'Samples 4
        GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              return win
          Nothing -> GLFW.terminate >> exitFailure
    where
        simpleErrorCallback e s =
            putStrLn $ unwords [show e, show s]

cleanup :: GLFW.Window -> IO ()
cleanup win = do
    GLFW.destroyWindow win
    GLFW.terminate
    exitSuccess

mainLoop :: IO () -> GLFW.Window -> IO ()
mainLoop draw w = do
    close <- GLFW.windowShouldClose w
    unless close $ do
                    draw
                    GLFW.swapBuffers w
                    GLFW.pollEvents
                    mainLoop draw w

processEvents :: HydraState -> IO ()
processEvents state = do
    let tc = eventsChan state
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do
          processEvent state e
          processEvents state
      Nothing -> return ()

processEvent :: HydraState -> Event -> IO ()
processEvent state ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          let win = window state
          liftIO $ GLFW.setWindowShouldClose win True
          
      (EventKey win k scancode ks mk) -> do
--          printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True
{-
      (EventWindowPos _ x y) ->
--          printEvent "window pos" [show x, show y]
          return ()

      (EventWindowSize _ width height) ->
--          printEvent "window size" [show width, show height]
          return ()

      (EventWindowClose _) ->
--          printEvent "window close" []
          return ()

      (EventWindowRefresh _) ->
--          printEvent "window refresh" []
          return ()

      (EventWindowFocus _ fs) ->
--          printEvent "window focus" [show fs]
          return ()

      (EventWindowIconify _ is) ->
--          printEvent "window iconify" [show is]
          return ()
-}
      (EventMouseButton _ mb mbs mk) -> do
--          printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
          return ()

      (EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
--          printEvent "cursor pos" [show x', show y']
          return ()

      (EventCursorEnter _ cs) ->
--          printEvent "cursor enter" [show cs]
          return ()
                
      (EventChar _ c) ->
--          printEvent "char" [show c]
          return ()


printEvent :: String -> [String] -> IO ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

