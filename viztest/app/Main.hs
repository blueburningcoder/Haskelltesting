module Main where

import Bindings
import Display
import Graphics.UI.GLUT
import Data.IORef 


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Hello World"
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  angle <- newIORef 0.0
  displayCallback $= display angle
  idleCallback $= Just (idle angle)
  mainLoop







