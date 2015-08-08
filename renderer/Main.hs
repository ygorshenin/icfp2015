module Main where

import Control.Monad
import Control.Monad.State
import Data.IORef
import Graphics.Rendering.OpenGL (($=), GLfloat)
import Text.Printf (printf)
import System.Environment
import System.Exit
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Core
import CommandLine

data GamesState = GameRunning
               | GameCompleted
               | GamesCompleted
                 deriving (Show, Eq)

data Step = BlockMoved Command
          | BlockLocked
          | CommandsCompleted
          | InvalidBlock
          | GamePaused
            deriving (Show, Eq)

data RendererState = RendererState { rsQuit      :: Bool
                                   , rsPause     :: Bool
                                   , rsCL        :: CommandLine
                                   , rsState     :: GamesState
                                   , rsInput     :: Input
                                   , rsOutputs   :: [Output]
                                   , rsUnits     :: [Unit]
                                   , rsFilled    :: [Cell]
                                   , rsCommands  :: [Command]
                                   , rsTimestamp :: Double
                                   }
                   deriving (Show, Eq)

rsCheckLock :: RendererState -> Bool
rsCheckLock = not . noCheckLock . rsCL

setGamesState :: GamesState -> RendererState -> RendererState
setGamesState state gs = gs { rsState = state }

onStepCompleted :: Step -> IORef RendererState -> IO ()
onStepCompleted CommandsCompleted rs = do
  putStrLn "Commands completed."
  modifyIORef rs (setGamesState GameCompleted)
onStepCompleted InvalidBlock rs = do
  putStrLn "Current unit is in invalid state."
  modifyIORef rs (setGamesState GameCompleted)
onStepCompleted _ _ = return ()

borderColor = (105, 105, 105)
circleColor = (85, 107, 47)
filledColor = (238, 232, 170)
unitColor   = (154, 205, 50)

timeoutSec = 0.2

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
      putStrLn "Quit"
      modifyIORef rendererState setQuit
  when (key == GLFW.CharKey ' ' && state == GLFW.Press) $ do
      result <- macroStep rendererState
      onStepCompleted result rendererState
      return ()
  when (key == GLFW.SpecialKey GLFW.ENTER && state == GLFW.Press) $ do
      nextState rendererState
      return ()
  when ((key == GLFW.CharKey 'p') || (key == GLFW.CharKey 'P') && state == GLFW.Press) $ do
      rs <- readIORef rendererState
      let paused = not $ rsPause rs
      if paused
      then putStrLn "Pause"
      else putStrLn "Resuming the game"
      writeIORef rendererState $ rs { rsPause = paused }
  when ((key == GLFW.SpecialKey GLFW.UP || key == GLFW.SpecialKey GLFW.DOWN) && state == GLFW.Press) $ do
      rs <- readIORef rendererState
      let (unit:units) = rsUnits rs
          rot = if key == GLFW.SpecialKey GLFW.UP then CCW else CW
          unit' = rotateUnit rot unit
      putStrLn $ "Rotating current unit " ++ show rot
      writeIORef rendererState $ rs { rsUnits = (unit':units) }

hexagon :: [(GLfloat, GLfloat)]
hexagon = [(cos angle, sin angle) | i <- [0 .. 5], let angle = pi / 6.0 + pi / 3.0 * i]

circle :: GLfloat -> [(GLfloat, GLfloat)]
circle radius = [(radius * (cos angle), radius * (sin angle)) | i <- [1 .. n], let angle = 2.0 * pi / n * i]
    where n = 16

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
      getCenterX (Cell col row) = -1.0 + dist * (fromIntegral col + if even row then 0.5 else 1.0)
      getCenterY (Cell _ row)   = 1.0 - 0.5 * dist * (1 + (sqrt 3.0) * (fromIntegral row))

      ix = [Cell col row | row <- [0 .. numRows - 1], col <- [0 .. numCols - 1]]

      drawPointsInCell mode color points cell = do
          let cx = getCenterX cell
              cy = getCenterY cell
          GL.preservingMatrix $ do
              translate cx cy
              scale (dist * 0.5) (dist * 0.5)
              colorInt color
              GL.renderPrimitive mode $ do
                  forM_ points $ \(x, y) -> vertex x y

  -- Draws the Honeycomb.
  forM_ ix $ drawPointsInCell GL.LineLoop borderColor hexagon

  -- Draws filled cells.
  forM_ (rsFilled rs) $ drawPointsInCell GL.Polygon filledColor hexagon

  -- Draws the current unit if it doesn't intersect with filled cells.
  let unit   = head $ rsUnits rs
      filled = rsFilled rs
  when (not $ isBlocked input filled unit) $ do
      forM_ (members unit) $ drawPointsInCell GL.Polygon unitColor hexagon
      drawPointsInCell GL.Polygon circleColor (circle 0.2) (pivot unit)
  return ()

display :: IORef RendererState -> IO ()
display rendererState = do
  GL.clearColor $= GL.Color4 0 0 0 1.0
  GL.clear [GL.ColorBuffer]

  GL.loadIdentity
  drawGrid rendererState

macroStep :: IORef RendererState -> IO Step
macroStep rendererState = do
  result <- step rendererState
  case result of
    (BlockMoved _) -> macroStep rendererState
    _              -> return result

step :: IORef RendererState -> IO Step
step rendererState = do
  rs <- readIORef rendererState
  ts <- GL.get GLFW.time

  if null $ rsCommands rs
  then return CommandsCompleted
  else if rsPause rs
  then return GamePaused
  else do
    let input = rsInput rs
        filled = rsFilled rs
        (unit:units) = rsUnits rs
        (command:commands) = rsCommands rs

        unit' = applyCommand command unit
    if command == LockCheck
    then do
      fail "Block is not locked."
    else if isBlocked input filled unit
    then return InvalidBlock
    else if isBlocked input filled unit'
    then do
      let filled' = removeFullRows input (filled ++ (members unit))
      let commands' = if rsCheckLock rs
                      then tail commands
                      else commands
      when (rsCheckLock rs && (head commands) /= LockCheck) $
         fail "Block is not locked."
      writeIORef rendererState $ rs { rsUnits = units
                                    , rsFilled = filled'
                                    , rsCommands = commands
                                    , rsTimestamp = ts
                                    }
      return BlockLocked
    else do
      writeIORef rendererState $ rs { rsUnits = (unit':units)
                                    , rsCommands = commands
                                    , rsTimestamp = ts
                                    }
      return $ BlockMoved command

rendererLoop :: IORef RendererState -> IO ()
rendererLoop rendererState = do
  let loop = do
        GLFW.pollEvents
        display rendererState
        GLFW.swapBuffers
        rs <- readIORef rendererState
        ts <- GL.get GLFW.time

        when (ts >= rsTimestamp rs + timeoutSec && rsState rs == GameRunning) $ do
            result <- step rendererState
            onStepCompleted result rendererState
        when (not $ rsQuit rs) $ loop
  loop

initState :: CommandLine -> Input -> [Output] -> IO RendererState
initState cl input outputs = do
  let output = head outputs
      pid = problemId output
      s   = seed output
  when (pid /= (problemId output)) $ do
      fail $ "Input id and output id mismatch: " ++ show pid ++ " vs. " ++ show (problemId output)

  printf "Initializing state for problem id: %d, seed: %d\n" pid s

  timestamp <- GL.get GLFW.time
  return $ RendererState { rsQuit      = False
                         , rsPause     = False
                         , rsCL        = cl
                         , rsState     = GameRunning
                         , rsInput     = input
                         , rsOutputs   = outputs
                         , rsUnits     = genUnits input s
                         , rsFilled    = filled input
                         , rsCommands  = solution output
                         , rsTimestamp = timestamp
                         }

nextState :: IORef RendererState -> IO ()
nextState rendererState = do
  rs <- readIORef rendererState
  let input   = rsInput rs
      outputs = rsOutputs rs
  if null (tail outputs)
  then putStrLn "This is the last game."
  else do
    putStrLn "Switching to the next game."
    rs' <- initState (rsCL rs) input (tail outputs)
    writeIORef rendererState rs'

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

  input  <- readJSON ip :: IO Input
  output <- readJSON op :: IO [Output]
  rs     <- initState commandLine input output

  rendererState <- newIORef rs

  GLFW.windowSizeCallback $= onResize
  GLFW.windowCloseCallback $= onClose rendererState
  GLFW.keyCallback $= onKey rendererState
  rendererLoop rendererState

  GLFW.terminate
