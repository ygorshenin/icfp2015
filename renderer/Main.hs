module Main where

import Control.Monad
import Control.Monad.State
import Data.IORef
import Graphics.Rendering.OpenGL (($=), GLfloat)
import System.Environment
import System.Exit
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Core
import CommandLine

data RendererState = RendererState { rsQuit   :: Bool
                                   , rsInput  :: Input
                                   , rsUnitIx :: Int
                                   , rsUnit   :: Unit
                                   }
                   deriving (Show, Eq)

unitColor   = (0, 255, 0)
filledColor = (238, 232, 170)

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

onResize :: GLFW.WindowSizeCallback
onResize size@(GL.Size width height) = do
  GL.viewport $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (realToFrac width) (realToFrac height) 0

onClose :: IORef RendererState -> GLFW.WindowCloseCallback
onClose rendererState = do
  modifyIORef rendererState setQuit
  return True

onKey :: IORef RendererState -> GLFW.KeyCallback
onKey rendererState key state = do
  when (key == GLFW.SpecialKey GLFW.ESC && state == GLFW.Press) $ do
      modifyIORef rendererState setQuit
  when (key == GLFW.CharKey ' ' && state == GLFW.Press) $ do
      rs <- readIORef rendererState
      let unitIx = succ $ rsUnitIx rs
          unit   = spawnUnit (rsInput rs) unitIx
      writeIORef rendererState $ rs { rsUnitIx = unitIx, rsUnit = unit }
      putStrLn $ "Switching to unit: " ++ show unitIx
hexagon :: [(GLfloat, GLfloat)]
hexagon = [(cos angle, sin angle) | i <- [0 .. 5], let angle = pi / 6.0 + pi / 3.0 * i]

colorInt :: (Int, Int, Int) -> IO ()
colorInt (r, g, b) = color (fromIntegral r / 255.0)
                           (fromIntegral g / 255.0)
                           (fromIntegral b / 255.0)

color :: GLfloat -> GLfloat -> GLfloat -> IO ()
color r g b = GL.color $ GL.Color3 r g b

vertex :: GLfloat -> GLfloat -> IO ()
vertex x y = GL.vertex $ GL.Vertex3 x y 0.0

translate :: GLfloat -> GLfloat -> IO ()
translate x y = GL.translate $ GL.Vector3 x y 0

scale :: GLfloat -> GLfloat -> IO ()
scale sx sy = GL.scale sx sy 1.0

drawHexagon :: GL.PrimitiveMode -> IO ()
drawHexagon mode = do
  GL.renderPrimitive mode $ do
      forM_ hexagon $ \(x, y) -> vertex x y

centersDist :: RendererState -> GLfloat
centersDist rendererState = min dx (2.0 * dy / sqrt 3.0)
    where numRows = height $ rsInput rendererState
          numCols = width $ rsInput rendererState
          dx = 2.0 / (fromIntegral numCols + 0.5) :: GLfloat
          dy = 2.0 / (fromIntegral numRows) :: GLfloat

drawGrid :: IORef RendererState -> IO ()
drawGrid rendererState = do
  rs <- readIORef rendererState
  let input = rsInput rs
      numRows = height input
      numCols = width input

      dist = centersDist rs
      getCenterX (row, col) = -1.0 + dist * (fromIntegral col + if even row then 0.5 else 1.0)
      getCenterY (row, _)   = 1.0 - 0.5 * dist * (1 + (sqrt 3.0) * (fromIntegral row))

      ix = [(row, col) | row <- [0 .. numRows - 1], col <- [0 .. numCols - 1]]

  -- Draws the Honeycomb.
  forM_ ix $ \coord -> do
    let cx = getCenterX coord
        cy = getCenterY coord
    GL.preservingMatrix $ do
      translate cx cy
      scale (dist / 2.0) (dist / 2.0)
      color 1.0 1.0 1.0
      drawHexagon GL.LineLoop

  -- Draws filled cells.
  forM_ (filled input) $ \(Cell col row) -> do
    let coord = (row, col)
        cx = getCenterX coord
        cy = getCenterY coord
    GL.preservingMatrix $ do
      translate cx cy
      scale (dist / 2.0) (dist / 2.0)
      colorInt filledColor
      drawHexagon GL.Polygon

  -- Draws the current unit.
  let unit = rsUnit rs
  forM_ (members unit) $ \(Cell col row) -> do
    let coord = (row, col)
        cx = getCenterX coord
        cy = getCenterY coord
    GL.preservingMatrix $ do
      translate cx cy
      scale (dist / 2.0) (dist / 2.0)
      colorInt unitColor
      drawHexagon GL.Polygon

  return ()

display :: IORef RendererState -> IO ()
display rendererState = do
  GL.clearColor $= GL.Color4 0 0 0 1.0
  GL.clear [GL.ColorBuffer]

  GL.loadIdentity
  drawGrid rendererState

rendererLoop :: IORef RendererState -> IO ()
rendererLoop rendererState = do
  let loop = do
        GLFW.pollEvents
        display rendererState
        GLFW.swapBuffers
        rs <- readIORef rendererState
        when (not $ rsQuit rs) $ loop
  loop

type CLI = State [String]

main :: IO ()
main = do

  runOrDie GLFW.initialize "GLFW: can't initialize"
  runOrDie (GLFW.openWindow
            (GL.Size windowWidth windowHeight)
            [GLFW.DisplayAlphaBits 8]
            GLFW.Window) $
       "GLFW: can't create window"
  GLFW.windowTitle $= "IFPC2015"

  commandLine <- parseCommandLine
  let ip = inputPath commandLine
  when (ip == "") $ fail "Input is not specified."

  input <- readInput ip
  rendererState <- newIORef $ RendererState False input 0 (spawnUnit input 0)

  GLFW.windowSizeCallback $= onResize
  GLFW.windowCloseCallback $= onClose rendererState
  GLFW.keyCallback $= onKey rendererState
  rendererLoop rendererState

  GLFW.terminate
