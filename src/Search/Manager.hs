{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Search.Manager (getManager) where

import Control.Concurrent (threadDelay)
import Data.Functor (($>))
import Network.HTTP.Client (
    Manager,
    ManagerSettings (..),
    Request (..),
    newManager,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)

getManager :: IO Manager
getManager =
    newManager
        tlsManagerSettings
            { managerModifyRequest = rateLimit
            }
  where
    rateLimit req =
        if h == "api.scryfall.com"
            then threadDelay 50_000 $> req
            else pure req
      where
        h = host req
