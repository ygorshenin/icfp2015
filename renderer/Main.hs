module Main where

import Control.Monad
import Data.IORef
import Graphics.Rendering.OpenGL (($=), GLfloat)
import System.Exit
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

data RendererState = RendererState { rsQuit :: Bool }
                   deriving (Show, Eq)

setQuit :: RendererState -> RendererState
setQuit rs = rs { rsQuit = True }

windowWidth = 800
windowHeight = 600

runOrDie :: IO Bool -> String -> IO ()
runOrDie action message = do
  ok <- action
  when (not ok) $ do
    GLFW.terminate
    putStrLn message
    exitFailure

onKey :: IORef RendererState -> GLFW.KeyCallback
onKey rendererState key state = do
  when (key == GLFW.SpecialKey GLFW.ESC && state == GLFW.Press) $ do
      modifyIORef rendererState setQuit

onClose :: IORef RendererState -> GLFW.WindowCloseCallback
onClose rendererState = do
  modifyIORef rendererState setQuit
  return True

hexagon :: [(GLfloat, GLfloat)]
hexagon = [(cos angle, sin angle) | i <- [0 .. 5], let angle = pi / 6.0 + pi / 3.0 * i]

color :: GLfloat -> GLfloat -> GLfloat -> IO ()
color r g b = GL.color $ GL.Color3 r g b

vertex :: GLfloat -> GLfloat -> IO ()
vertex x y = GL.vertex $ GL.Vertex3 x y 0.0

translate :: GLfloat -> GLfloat -> IO ()
translate x y = GL.translate $ GL.Vector3 x y 0

scale :: GLfloat -> GLfloat -> IO ()
scale sx sy = GL.scale sx sy 1.0

drawHexagon :: IO ()
drawHexagon = do
  GL.renderPrimitive GL.LineLoop $ do
                color 1.0 1.0 1.0
                forM_ hexagon $ \(x, y) -> vertex x y

drawGrid :: (Int, Int) -> IO ()
drawGrid (numRows, numCols) = do
  let dx = 2.0 / (fromIntegral numCols + 0.5) :: GLfloat
      dy = 2.0 / (fromIntegral numRows) :: GLfloat
      dist = min dx (2.0 * dy / sqrt 3.0)
      distX = dist
      distY = dist * (sqrt 3.0) / 2.0
      ix = [(row, col) | row <- [0 .. numRows - 1], col <- [0 .. numCols - 1]]
  forM_ ix $ \(row, col) -> do
    let cx = -1.0 + dist / 2.0 + distX * (fromIntegral col) + (if even row then 0 else distX / 2.0)
        cy = 1.0 - dist / 2.0 - distY * (fromIntegral row)
    GL.preservingMatrix $ do
      translate cx cy
      scale (dist / 2.0) (dist / 2.0)
      drawHexagon
  return ()

display :: IO ()
display = do
  GL.clearColor $= GL.Color4 0 0 0 1.0
  GL.clear [GL.ColorBuffer]

  GL.loadIdentity
  drawGrid (20, 10)

rendererLoop :: IORef RendererState -> IO ()
rendererLoop rendererState = do
  let loop = do
        GLFW.pollEvents
        display
        GLFW.swapBuffers
        rs <- readIORef rendererState
        when (not $ rsQuit rs) $ loop
  loop

main :: IO ()
main = do
  runOrDie GLFW.initialize "GLFW: can't initialize"
  runOrDie (GLFW.openWindow
            (GL.Size windowWidth windowHeight)
            [GLFW.DisplayAlphaBits 8]
            GLFW.Window) $
       "GLFW: can't create window"
  GLFW.windowTitle $= "IFPC2015"

  rendererState <- newIORef $ RendererState False
  GLFW.windowCloseCallback $= onClose rendererState
  GLFW.keyCallback $= onKey rendererState
  rendererLoop rendererState

  GLFW.terminate
