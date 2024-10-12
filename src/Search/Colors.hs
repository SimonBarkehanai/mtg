{-# LANGUAGE OverloadedStrings #-}

module Search.Colors (Color (..), Colors) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson (Value (String))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.String (IsString)
import Data.Text (append, unpack)

type Colors = [Color]
data Color = White | Blue | Black | Red | Green | Colorless deriving (Show, Eq)

toString :: (IsString a) => Color -> a
toString White = "W"
toString Blue = "U"
toString Black = "B"
toString Red = "R"
toString Green = "G"
toString Colorless = "C"

fromString :: (IsString a, Eq a) => a -> Maybe Color
fromString "W" = Just White
fromString "U" = Just Blue
fromString "B" = Just Black
fromString "R" = Just Red
fromString "G" = Just Green
fromString "C" = Just Colorless
fromString "w" = Just White
fromString "u" = Just Blue
fromString "b" = Just Black
fromString "r" = Just Red
fromString "g" = Just Green
fromString "c" = Just Colorless
fromString _ = Nothing

instance ToJSON Color where
    toJSON c = Aeson.String $ toString c
instance FromJSON Color where
    parseJSON (Aeson.String s) =
        maybe
            (fail $ unpack $ "Unknown Color: " `append` s)
            pure
            (fromString s)
    parseJSON x =
        prependFailure "Parsing Color failed: " $
            typeMismatch "String" x
