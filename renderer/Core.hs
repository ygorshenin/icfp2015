{-# LANGUAGE OverloadedStrings #-}
module Core ( Cell (..)
            , Unit (..)
            , Input (..)
            , readInput
            , cellAdd
            , getUnitOffset
            ) where

import Data.Aeson
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Control.Monad

data Cell = Cell { cellX :: Int, cellY :: Int }
          deriving (Show, Eq)

cellAdd :: Cell -> Cell -> Cell
cellAdd (Cell x1 y1) (Cell x2 y2) = Cell (x1 + x2) (y1 + y2)

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

-- Returns offset for a unit
getUnitOffset :: Input -> Int -> Cell
getUnitOffset input unit = Cell { cellX = x, cellY = y }
    where numCols = width input
          ms      = members $ (units input) !! unit
          topMost   = cellY $ minimumBy (compare `on` cellY) ms
          leftMost  = cellX $ minimumBy (compare `on` cellX) ms
          rightMost = cellX $ maximumBy (compare `on` cellX) ms
          y = -topMost
          xlim = numCols - leftMost - rightMost - 1
          x = if xlim < 0 then (xlim + 1) `div` 2 else xlim `div` 2

instance FromJSON Cell where
    parseJSON (Object v) = Cell <$>
                           v .: "x" <*> v .: "y"

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

readInput :: FilePath -> IO Input
readInput path = liftM (fromJust . decode) $ L.readFile path
