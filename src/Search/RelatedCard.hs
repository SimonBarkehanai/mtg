{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Search.RelatedCard (RelatedCard (..), Component (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericToEncoding)
import qualified Data.Aeson as Aeson (Value (String))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.String (IsString)
import Data.Text (append, unpack)
import GHC.Generics (Generic)
import Search.Alias (URI, UUID)

data RelatedCard = RelatedCard
    { id :: UUID
    , component :: Component
    , name :: String
    , type_line :: String
    , uri :: URI
    }
    deriving (Generic, Show, Eq)

instance ToJSON RelatedCard where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RelatedCard

data Component = Token | MeldPart | MeldResult | ComboPiece
    deriving (Show, Eq)

toString :: (IsString a) => Component -> a
toString Token = "token"
toString MeldPart = "meld_part"
toString MeldResult = "meld_result"
toString ComboPiece = "combo_piece"

fromString :: (IsString a, Eq a) => a -> Maybe Component
fromString "token" = Just Token
fromString "meld_part" = Just MeldPart
fromString "meld_result" = Just MeldResult
fromString "combo_piece" = Just ComboPiece
fromString _ = Nothing

instance ToJSON Component where
    toJSON c = Aeson.String $ toString c

instance FromJSON Component where
    parseJSON (Aeson.String s) =
        maybe
            (fail $ unpack $ "Unknown Component: " `append` s)
            pure
            (fromString s)
    parseJSON x =
        prependFailure "Parsing Component failed: " $
            typeMismatch "String" x
