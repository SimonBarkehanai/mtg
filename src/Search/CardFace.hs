{-# LANGUAGE DeriveGeneric #-}

module Search.CardFace (CardFace (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericToEncoding)
import GHC.Generics (Generic)
import Search.Alias (Decimal, UUID)
import Search.Colors (Colors)
import Search.ImageURIs (ImageURIs (..))

data CardFace = CardFace
    { artist :: Maybe String
    , artist_id :: Maybe UUID
    , cmc :: Maybe Decimal
    , color_indicator :: Maybe Colors
    , colors :: Maybe Colors
    , defense :: Maybe String
    , flavor_text :: Maybe String
    , illustration_id :: Maybe UUID
    , image_uris :: Maybe ImageURIs
    , layout :: Maybe String
    , loyalty :: Maybe String
    , mana_cost :: String
    , name :: String
    , oracle_id :: Maybe UUID
    , oracle_text :: Maybe String
    , power :: Maybe String
    , printed_name :: Maybe String
    , printed_text :: Maybe String
    , printed_type_line :: Maybe String
    , toughness :: Maybe String
    , type_line :: Maybe String
    , watermark :: Maybe String
    }
    deriving (Generic, Show, Eq)

instance ToJSON CardFace where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CardFace
