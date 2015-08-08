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

data RendererState = RendererState { rsQuit      :: Bool
                                   , rsInput     :: Input
                                   , rsOutput    :: [Output]
                                   , rsUnits     :: [Unit]
                                   , rsFilled    :: [Cell]
                                   , rsCommands  :: [Command]
                                   , rsTimestamp :: Double
                                   }
                   deriving (Show, Eq)

unitColor   = (0, 255, 0)
filledColor = (238, 232, 170)

timeoutSec = 0.1

setQuit :: RendererState -> RendererState
setQuit rs = rs { rsQuit = True }

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
  forM_ (rsFilled rs) $ \(Cell col row) -> do
    let coord = (row, col)
        cx = getCenterX coord
        cy = getCenterY coord
    GL.preservingMatrix $ do
      translate cx cy
      scale (dist / 2.0) (dist / 2.0)
      colorInt filledColor
      drawHexagon GL.Polygon

  -- Draws the current unit.
  let unit = head $ rsUnits rs
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

step :: IORef RendererState -> IO ()
step rendererState = do
  rs <- readIORef rendererState
  ts <- GL.get GLFW.time

  let input = rsInput rs
      filled = rsFilled rs
      (unit:units) = rsUnits rs
      (command:commands) = rsCommands rs

      unit' = applyCommand command unit
  if isBlocked input filled unit'
  then do
    writeIORef rendererState $ rs { rsUnits = units
                                  , rsFilled = filled ++ (members unit)
                                  , rsCommands = commands
                                  , rsTimestamp = ts
                                  }
  else do
    writeIORef rendererState $ rs { rsUnits = (unit':units)
                                  , rsCommands = commands
                                  , rsTimestamp = ts
                                  }

rendererLoop :: IORef RendererState -> IO ()
rendererLoop rendererState = do
  let loop = do
        GLFW.pollEvents
        display rendererState
        GLFW.swapBuffers
        rs <- readIORef rendererState
        ts <- GL.get GLFW.time

        when (ts >= rsTimestamp rs + timeoutSec) $ step rendererState
        when (not $ rsQuit rs) $ loop
  loop

type CLI = State [String]

main :: IO ()
main = do
  commandLine <- parseCommandLine

  runOrDie GLFW.initialize "GLFW: can't initialize"
  runOrDie (GLFW.openWindow
            (GL.Size(fromIntegral $ windowWidth commandLine) (fromIntegral $ windowHeight commandLine))
            [GLFW.DisplayAlphaBits 8]
            GLFW.Window) $
       "GLFW: can't create window"
  GLFW.windowTitle $= "IFPC2015"

  let ip = inputPath commandLine
      op = outputPath commandLine
  when (ip == "") $ fail "Input is not specified (-i option)."
  when (op == "") $ fail "Output is not specified (-o option)."

  input <- readJSON ip :: IO Input
  output <- readJSON op :: IO [Output]
  timestamp <- GL.get GLFW.time
  rendererState <- newIORef $ RendererState { rsQuit   = False
                                            , rsInput  = input
                                            , rsOutput = output
                                            , rsUnits  = genUnits input (head $ sourceSeeds input)
                                            , rsFilled = filled input
                                            , rsCommands = solution (head output)
                                            , rsTimestamp = timestamp
                                            }

  GLFW.windowSizeCallback $= onResize
  GLFW.windowCloseCallback $= onClose rendererState
  GLFW.keyCallback $= onKey rendererState
  rendererLoop rendererState

  GLFW.terminate
