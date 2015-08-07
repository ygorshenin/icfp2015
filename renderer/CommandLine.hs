module CommandLine (CommandLine(..), parseCommandLine) where

import System.Environment (getArgs)

data CommandLine = CommandLine { inputPath :: FilePath }
                   deriving (Show, Eq)

parseArgs :: [String] -> CommandLine -> CommandLine
parseArgs [] cl = cl
parseArgs (s:ss) cl = case s of
                        "-i" -> parseArgs (tail ss) $ cl { inputPath = head ss }
                        _    -> error $ "Unknown arg:" ++ show s

parseCommandLine :: IO CommandLine
parseCommandLine = do
  args <- getArgs
  return . parseArgs args $ CommandLine { inputPath = "" }
