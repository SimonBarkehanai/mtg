{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Search.Card (
    Card (..),
    Legality (..),
    Legalities (..),
    Finish (..),
    FrameEffect (..),
    Game (..),
    Prices (..),
    PurchaseURIs (..),
    RelatedURIs (..),
    Preview (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericToEncoding)
import qualified Data.Aeson as Aeson (Value (String))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Map (Map)
import Data.Text (append, unpack)
import GHC.Generics (Generic)
import Search.Alias (Date, Decimal, URI, UUID)
import Search.CardFace (CardFace)
import Search.Colors (Colors)
import Search.ImageURIs (ImageURIs (..))
import Search.RelatedCard (RelatedCard)

data Card = Card
    { -- Core Fields
      arena_id :: Maybe Int
    , id :: UUID
    , lang :: String
    , mtgo_id :: Maybe Int
    , mtgo_foil_id :: Maybe Int
    , multiverse_ids :: Maybe [Int]
    , tcgplayer_id :: Maybe Int
    , tcgplayer_etched_id :: Maybe Int
    , cardmarket_id :: Maybe Int
    , layout :: String
    , oracle_id :: Maybe String
    , prints_search_uri :: URI
    , rulings_uri :: URI
    , scryfall_uri :: URI
    , uri :: URI
    , -- Gameplay Fields
      all_parts :: Maybe [RelatedCard]
    , card_faces :: Maybe [CardFace]
    , cmc :: Maybe Decimal
    , color_identity :: Colors
    , color_indicator :: Maybe Colors
    , colors :: Maybe Colors
    , defense :: Maybe String
    , edhrec_rank :: Maybe Int
    , hand_modifier :: Maybe String
    , keywords :: [String]
    , legalities :: Legalities
    , life_modifier :: Maybe String
    , loyalty :: Maybe String
    , mana_cost :: Maybe String
    , name :: String
    , oracle_text :: Maybe String
    , penny_rank :: Maybe Int
    , power :: Maybe String
    , produced_mana :: Maybe Colors
    , reserved :: Bool
    , toughness :: Maybe String
    , type_line :: Maybe String
    , -- Print Fields
      artist :: Maybe String
    , artist_ids :: Maybe [UUID]
    , attraction_lights :: Maybe [Int]
    , booster :: Bool
    , border_color :: String
    , card_back_id :: Maybe UUID
    , collector_number :: String
    , content_warning :: Maybe Bool
    , digital :: Bool
    , finishes :: [Finish]
    , flavor_name :: Maybe String
    , flavor_text :: Maybe String
    , frame_effects :: Maybe [FrameEffect]
    , frame :: String
    , full_art :: Bool
    , games :: [Game]
    , highres_image :: Bool
    , illustration_id :: Maybe UUID
    , image_status :: String
    , image_uris :: Maybe ImageURIs
    , oversized :: Bool
    , prices :: Prices
    , printed_name :: Maybe String
    , printed_text :: Maybe String
    , printed_type_line :: Maybe String
    , promo :: Bool
    , promo_types :: Maybe [String]
    , purchase_uris :: Maybe PurchaseURIs
    , rarity :: String
    , related_uris :: RelatedURIs
    , released_at :: Date
    , reprint :: Bool
    , scryfall_set_uri :: URI
    , set_name :: String
    , set_search_uri :: URI
    , set_type :: String
    , set_uri :: URI
    , set :: String
    , set_id :: UUID
    , story_spotlight :: Bool
    , textless :: Bool
    , variation :: Bool
    , variation_of :: Maybe UUID
    , security_stamp :: Maybe String
    , watermark :: Maybe String
    , preview :: Maybe Preview
    }
    deriving (Generic, Show, Eq)

instance ToJSON Card where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Card

data Legality = Legal | NotLegal | Restricted | Banned deriving (Generic, Show, Eq)
instance ToJSON Legality where
    toJSON Legal = Aeson.String "legal"
    toJSON NotLegal = Aeson.String "not_legal"
    toJSON Restricted = Aeson.String "restricted"
    toJSON Banned = Aeson.String "banned"
instance FromJSON Legality where
    parseJSON (Aeson.String "legal") = pure Legal
    parseJSON (Aeson.String "not_legal") = pure NotLegal
    parseJSON (Aeson.String "restricted") = pure Restricted
    parseJSON (Aeson.String "banned") = pure Banned
    parseJSON (Aeson.String s) =
        fail $
            unpack $
                "Unknown Legality: " `append` s
    parseJSON x =
        prependFailure "parsing Legality failed: " $
            typeMismatch "String" x

newtype Legalities = Legalities (Map String Legality) deriving (Generic, Show, Eq)
instance ToJSON Legalities where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Legalities

data Finish = Foil | NonFoil | Etched deriving (Generic, Show, Eq)
instance ToJSON Finish where
    toJSON Foil = Aeson.String "foil"
    toJSON NonFoil = Aeson.String "nonfoil"
    toJSON Etched = Aeson.String "etched"
instance FromJSON Finish where
    parseJSON (Aeson.String "foil") = pure Foil
    parseJSON (Aeson.String "nonfoil") = pure NonFoil
    parseJSON (Aeson.String "etched") = pure Etched
    parseJSON (Aeson.String s) =
        fail $
            unpack $
                    "Unknown Finish: " `append` s
    parseJSON x =
        prependFailure "parsing Finish failed: " $
            typeMismatch "String" x

newtype FrameEffect = FrameEffect String deriving (Generic, Show, Eq)
instance ToJSON FrameEffect where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON FrameEffect

data Game = Paper | Arena | MTGO deriving (Generic, Show, Eq)
instance ToJSON Game where
    toJSON Paper = Aeson.String "paper"
    toJSON Arena = Aeson.String "arena"
    toJSON MTGO = Aeson.String "mtgo"
instance FromJSON Game where
    parseJSON (Aeson.String "paper") = pure Paper
    parseJSON (Aeson.String "arena") = pure Arena
    parseJSON (Aeson.String "mtgo") = pure MTGO
    parseJSON (Aeson.String s) =
        fail $
            unpack $
                    "Unknown Game: " `append` s
    parseJSON x =
        prependFailure "parsing Game failed: " $
            typeMismatch "String" x

newtype Prices = Prices (Map String (Maybe String)) deriving (Generic, Show, Eq)
instance ToJSON Prices where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Prices
newtype PurchaseURIs = PurchaseURIs (Map String String) deriving (Generic, Show, Eq)
instance ToJSON PurchaseURIs where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON PurchaseURIs
newtype RelatedURIs = RelatedURIs (Map String String) deriving (Generic, Show, Eq)
instance ToJSON RelatedURIs where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RelatedURIs

data Preview = Preview
    { previewed_at :: Maybe Date
    , source_uri :: Maybe URI
    , source :: Maybe String
    }
    deriving (Generic, Show, Eq)
instance ToJSON Preview where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Preview
