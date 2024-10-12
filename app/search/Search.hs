{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Search.Card as C (Card (..))
import Data.ByteString.Lazy as LBS (ByteString, intercalate, toStrict)
import Data.Char (toUpper)
import Data.List (isPrefixOf)
import Search.Manager (getManager)
import Options.Applicative
import Search.Search (search)
import Control.Monad.Trans.Except (runExceptT)

data Options = Options
    { query :: [ByteString]
    , forge :: Bool
    }

options :: Parser Options
options =
    Options
        <$> some
            ( argument
                str
                ( metavar "QUERY"
                    <> help "Query to search on Scryfall"
                )
            )
        <*> switch
            ( long "forge"
                <> short 'f'
                <> help "Output Forge format (Name|Set)"
            )

find :: Eq a => [a] -> [a] -> Maybe Int
find haystack needle = go haystack 0
  where
    go hs@(_ : t) n
        | needle `isPrefixOf` hs = Just n 
        | otherwise = go t (succ n)
    go [] _ = Nothing

output :: Bool -> [C.Card] -> String
output f cards = unlines $ map disp cards
  where
    disp C.Card{C.name = n, C.set = s}
        | f = n' ++ "|" ++ s'
        | otherwise = n
      where
        s' = map toUpper s
        n' = fixName n
    fixName n = case n `find` "//" of
        Just pos -> take (pred pos) n
        Nothing -> n

main :: IO ()
main = do
    o <- execParser opts
    let q = toStrict $ LBS.intercalate " " $ query o
        f = forge o
    putStrLn $ "q = " ++ show q
    putStrLn $ "f = " ++ show f
    manager <- getManager
    cards <- runExceptT $ search manager q
    putStrLn $ either id (output f) cards
    pure ()
  where
    opts =
        info
            (options <**> helper)
            ( fullDesc
                <> progDesc "Search Scryfall for QUERY"
                <> header "scryfall-search - a searcher for Scryfall"
            )
