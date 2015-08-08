module CommandLine (CommandLine(..), parseCommandLine) where

import System.Environment (getArgs)

data CommandLine = CommandLine { inputPath :: FilePath
                               , outputPath :: FilePath
                               , windowHeight :: Int
                               , windowWidth  :: Int
                               , noCheckLock  :: Bool
                               }
                   deriving (Show, Eq)

parseArgs :: [String] -> CommandLine -> CommandLine
parseArgs [] cl = cl
parseArgs (s:ss) cl = case s of
                        "-i" -> parseArgs (tail ss) $ cl { inputPath = head ss }
                        "-o" -> parseArgs (tail ss) $ cl { outputPath = head ss }
                        "-w" -> parseArgs (tail ss) $ cl { windowWidth = read $ head ss }
                        "-h" -> parseArgs (tail ss) $ cl { windowHeight = read $ head ss }
                        "-n" -> parseArgs ss $ cl { noCheckLock = True }
                        _    -> error $ "Unknown arg:" ++ show s

parseCommandLine :: IO CommandLine
parseCommandLine = do
  args <- getArgs
  return . parseArgs args $ CommandLine { inputPath = ""
                                        , outputPath = ""
                                        , windowHeight = 800
                                        , windowWidth = 600
                                        , noCheckLock = False
                                        }
