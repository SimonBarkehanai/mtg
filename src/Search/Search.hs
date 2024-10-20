{-# LANGUAGE OverloadedStrings #-}

module Search.Search (search) where

import Control.Monad.Trans.Except (ExceptT (..), except, withExceptT)
import Data.Aeson (
    Result (..),
    Value (..),
    eitherDecode,
    fromJSON,
    (.:),
    (.:?),
 )
import Data.Aeson.Types (parseEither)
import Data.ByteString as B (ByteString, null, toStrict, unpack)
import Data.ByteString.Builder (Builder, byteString, intDec, toLazyByteString, word8)
import Data.ByteString.Lazy as LB (fromChunks)
import Data.CaseInsensitive (mk)
import Data.Char (ord)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (
    BodyReader,
    Manager,
    Request (..),
    Response (..),
    brRead,
    defaultRequest,
    withResponse,
 )
import Search.Card (Card)

encodeQueryBuilder :: ByteString -> Builder
encodeQueryBuilder q = mconcat $ map encode $ unpack q
  where
    encode c
        | shouldEscape c = word8 37 {- % -} <> word8 (hex hi) <> word8 (hex lo)
        | otherwise = word8 c
      where
        (hi, lo) = c `divMod` 16
    shouldEscape c
        | c >= 0x41 {- A -} && c <= 0x5A {- Z -} = False
        | c >= 0x61 {- a -} && c <= 0x7A {- z -} = False
        | c >= 0x30 {- 0 -} && c <= 0x39 {- 9 -} = False
        | otherwise = c `notElem` extraToKeep
    hex x
        | x < 10 = 0x30 {- 0 -} + x
        | otherwise = 0x41 {- A -} + x - 10
    extraToKeep = map (fromIntegral . ord) "-_.~"

search :: Manager -> ByteString -> ExceptT String IO [Card]
search manager q = search' manager req
  where
    req =
        defaultRequest
            { path = "/cards/search"
            , host = "api.scryfall.com"
            , queryString = toStrict $ toLazyByteString $ "?q=" <> encodeQueryBuilder q
            , secure = True
            , port = 443
            }

search' :: Manager -> Request -> ExceptT String IO [Card]
search' manager req = go 1
  where
    go pg = do
        j <- getJSON manager $ page pg
        d <-
            withExceptT
                ( \e ->
                    "Error in query "
                        ++ show
                            (queryString req)
                        ++ " page "
                        ++ show pg
                        ++ ": "
                        ++ e
                )
                $ except
                $ jsonData j
        c <- except $ fixResults $ map fromJSON d
        rest <- if hasMore j then go (pg + 1) else pure []
        pure $ c ++ rest
    page pg = req{queryString = toStrict $ toLazyByteString (builder <> intDec pg)}
    builder = byteString (queryString req) <> "&page="

fixResults :: [Result a] -> Either String [a]
fixResults = foldr fixer (Right [])
  where
    fixer (Error e) _ = Left e
    fixer (Success y) ys = liftA2 (:) (Right y) ys -- ((:) <$> Right y) <*> ys

jsonData :: Value -> Either String [Value]
jsonData (Object o) =
    parseEither
        ( \obj -> do
            object <- obj .: "object"
            code <- obj .:? "code"
            if (object :: String) == "error" then fail $ "response is error: " ++ fromMaybe "Error" code else obj .: "data"
        )
        o
jsonData _ = Left "Response is not an object"

hasMore :: Value -> Bool
hasMore (Object o) =
    fromRight False $
        parseEither
            (.: "has_more")
            o
hasMore _ = False

getJSON :: Manager -> Request -> ExceptT String IO Value
getJSON manager req =
    ExceptT $
        withResponse
            (setHeader (setHeader req "Accept" "application/json") "User-Agent" "MTG-ScryfallAPI")
            manager
            ( \res ->
                do
                    let body = responseBody res
                    wholeBody <- allChunks body
                    return $ eitherDecode $ fromChunks wholeBody
            )

allChunks :: BodyReader -> IO [ByteString]
allChunks body = do
    let body' = brRead body
    chunk <- body'
    if B.null chunk then pure [] else fmap (chunk :) (allChunks body')

setHeader :: Request -> ByteString -> ByteString -> Request
setHeader req name value = req{requestHeaders = headers''}
  where
    name' = mk name
    headers = requestHeaders req
    headers' = filter (\(h, _) -> h /= name') headers
    headers'' = (name', value) : headers'
