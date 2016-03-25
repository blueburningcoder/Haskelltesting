module Bindings
    ( reshape, keyboardMouse
    ) where

import Graphics.UI.GLUT
import Display
import Data.IORef

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)


keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse a p key down _ _ = case key of
  (Char ' ') -> a $~! negate
  (Char '+') -> a $~! (*2)
  (Char '-') -> a $~! (/2)
  (SpecialKey KeyLeft)  -> p $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp)    -> p $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyDown)  -> p $~! \(x,y) -> (x,y-0.1)
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()


