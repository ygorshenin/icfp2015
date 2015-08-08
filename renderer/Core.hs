{-# LANGUAGE OverloadedStrings #-}
module Core ( Cell (..)
            , Unit (..)
            , Input (..)
            , readInput
            ) where

import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Control.Monad

data Cell = Cell { cellX :: Int, cellY :: Int }
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
