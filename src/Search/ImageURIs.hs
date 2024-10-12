{-# LANGUAGE DeriveGeneric #-}

module Search.ImageURIs (ImageURIs (..)) where

import Data.Aeson (
    FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
 )
import GHC.Generics (Generic)
import Search.Alias (URI)

data ImageURIs = ImageURIs
    { small :: URI
    , normal :: URI
    , large :: URI
    , png :: URI
    , art_crop :: URI
    , border_crop :: URI
    }
    deriving (Generic, Show, Eq)
instance ToJSON ImageURIs where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ImageURIs
