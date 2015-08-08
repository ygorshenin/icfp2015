{-# LANGUAGE OverloadedStrings #-}
module Core ( Cell (..)
            , Unit (..)
            , Input (..)
            , Output (..)
            , Command (..)
            , Rotation (..)
            , readJSON
            , genUnits
            , applyCommand
            , isBlocked
            , removeFullRows
            , rotateUnit
            ) where

import Data.Aeson
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Control.Monad

data Cell = Cell { cellX :: Int
                 , cellY :: Int
                 }
          deriving (Show, Eq)

data Unit = Unit { members :: [Cell]
                 , pivot :: Cell
                 }
            deriving (Show, Eq)

data Input = Input { id :: Int
                   , units :: [Unit]
                   , width :: Int
                   , height :: Int
                   , filled :: [Cell]
                   , sourceLength :: Int
                   , sourceSeeds :: [Int]
                   }
                 deriving (Show, Eq)

data Output = Output { problemId :: Int
                     , seed :: Int
                     , tag :: String
                     , solution :: [Command]
                     } deriving (Show, Eq)

data Rotation = CW | CCW deriving (Show, Eq)

data Command = MoveW
             | MoveE
             | MoveSW
             | MoveSE
             | Rotate Rotation
             | LockCheck
               deriving (Show, Eq)

cellAdd, cellSub :: Cell -> Cell -> Cell
cellAdd (Cell x1 y1) (Cell x2 y2) = Cell (x1 + x2) (y1 + y2)
cellSub (Cell x1 y1) (Cell x2 y2) = Cell (x1 - x2) (y1 - y2)

cellToCube :: Cell -> (Int, Int, Int)
cellToCube (Cell col row) = (x, y, z)
    where x = row
          z = col - (row + ((row + 1) `mod` 2)) `div` 2
          y = -x - z

cubeToCell :: (Int, Int, Int) -> Cell
cubeToCell (x, y, z) = Cell (z + (x + ((x + 1) `mod` 2)) `div` 2) x

rotateCube CCW (x, y, z) = (-z, -x, -y)
rotateCube CW (x, y, z)  = (-y, -z, -x)

cubeAdd (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
cubeSub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

rotateCell rot pv cc = cubeToCell . cubeAdd pvc . rotateCube rot $ cubeSub ccc pvc
    where ccc = cellToCube cc
          pvc = cellToCube pv

rotateUnit :: Rotation -> Unit -> Unit
rotateUnit rot (Unit ms pv) = Unit (map (rotateCell rot pv) ms) pv

parseCommand :: Char -> Command
parseCommand c | c `elem` moveW  = MoveW
               | c `elem` moveE  = MoveE
               | c `elem` moveSW = MoveSW
               | c `elem` moveSE = MoveSE
               | c `elem` rcw    = Rotate CW
               | c `elem` rccw   = Rotate CCW
               | c == '\n'       = LockCheck
               | otherwise       = error $ "Unknown command:" ++ show c
    where moveW  = "p'!.03"
          moveE  = "bcefy2"
          moveSW = "aghij4"
          moveSE = "lmno 5"
          rcw    = "dqrvz1"
          rccw   = "kstuwx"

-- Returns offset for a unit.
getUnitOffset :: Input -> Unit -> Cell
getUnitOffset input (Unit ms _) = Cell { cellX = x, cellY = y }
    where numCols = width input
          topMost   = cellY $ minimumBy (compare `on` cellY) ms
          leftMost  = cellX $ minimumBy (compare `on` cellX) ms
          rightMost = cellX $ maximumBy (compare `on` cellX) ms
          y = -topMost
          xlim = numCols - leftMost - rightMost - 1
          x = if xlim < 0 then (xlim + 1) `div` 2 else xlim `div` 2

-- Shifts unit by a given offset.
shiftUnit :: Unit -> Cell -> Unit
shiftUnit (Unit ms pv) c = Unit (map shift ms) (shift pv)
    where shift = cellAdd c

-- Spawns unit on an initial position.
spawnUnit :: Input -> Unit -> Unit
spawnUnit input unit = shiftUnit unit offset
    where offset = getUnitOffset input unit

-- Applies a given command to a unit.
applyCommand :: Command -> Unit -> Unit
applyCommand MoveE unit = shiftUnit unit Cell { cellX = 1, cellY = 0 }
applyCommand MoveW unit = shiftUnit unit Cell { cellX = -1, cellY = 0 }
applyCommand MoveSE (Unit ms pv) = Unit (map shift ms) (shift pv)
    where shift (Cell x y) = if even y
                             then (Cell x (y + 1))
                             else (Cell (x + 1) (y + 1))
applyCommand MoveSW (Unit ms pv) = Unit (map shift ms) (shift pv)
    where shift (Cell x y) = if even y
                             then (Cell (x - 1) (y + 1))
                             else (Cell x (y + 1))
applyCommand (Rotate rot) unit = rotateUnit rot unit

nextRand :: (Int, Int) -> (Int, Int)
nextRand (_, x) = (fromIntegral $ (x' `quot` 2 ^ 16) `mod` 2 ^ 15, fromIntegral $ x')
    where mdl = 2 ^ 32 :: Integer
          mul = 1103515245 :: Integer
          inc = 12345 :: Integer
          x'  = ((fromIntegral x) * mul + inc) `mod` mdl :: Integer

genRands :: Int -> [Int]
genRands seed = map fst $ iterate nextRand (0, seed)

genUnits :: Input -> Int -> [Unit]
genUnits input seed = map (\ix -> spawnUnit input $ us !! (ix `mod` ns)) $ genRands seed
    where us = units input
          ns = length us

-- Returns true when unit is in a blocked state.
isBlocked :: Input -> [Cell] -> Unit -> Bool
isBlocked input filled unit = inFilled || outOfBorder
    where w = width input
          h = height input
          ms = members unit
          inFilled = not . null $ intersect filled ms
          outOfBorder = any invalidCell ms

          invalidCell (Cell x y) = x < 0 || x >= w || y < 0 || y >= h

removeFullRows :: Input -> [Cell] -> [Cell]
removeFullRows input cells = concat . fst $ foldl update ([], 0) rows
    where rows = groupBy ((==) `on` cellY) . reverse $ sortBy (compare `on` cellY) cells

          update :: ([[Cell]], Int) -> [Cell] -> ([[Cell]], Int)
          update (rows, offset) row | length row == width input = (rows, offset + 1)
                                    | otherwise = (map (cellAdd (Cell 0 offset)) row : rows, offset)

instance FromJSON Cell where
    parseJSON (Object v) = Cell <$>
                           v .: "x" <*>
                           v .: "y"

instance FromJSON Unit where
    parseJSON (Object v) = Unit <$>
                           v .: "members" <*>
                           v .: "pivot"
    parseJSON _ = mzero

instance FromJSON Input where
    parseJSON (Object v) = Input <$>
                           v .: "id" <*>
                           v .: "units" <*>
                           v .: "width" <*>
                           v .: "height" <*>
                           v .: "filled" <*>
                           v .: "sourceLength" <*>
                           v .: "sourceSeeds"
    parseJSON _ = mzero

instance FromJSON Output where
    parseJSON (Object v) = Output <$>
                           v .: "problemId" <*>
                           v .: "seed" <*>
                           v .: "tag" <*>
                           liftM (map parseCommand) (v .: "solution")

readJSON :: FromJSON a => FilePath -> IO a
readJSON = liftM (fromJust . decode) . L.readFile
