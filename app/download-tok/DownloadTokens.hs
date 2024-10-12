module Main (main) where

import DownloadImages.Download (getDownloader, tokenDownload)
import System.Environment (getArgs, getProgName)
import Control.Monad.Trans.Except(runExceptT)

help :: IO ()
help = do
    progName <- getProgName
    putStrLn "Usage:"
    putStrLn $ '\t' : progName ++ " <forge directory>"

main :: IO ()
main = do
    args <- getArgs
    case args of
        (forgeDir : _) -> do
            res <- runExceptT $ do
                d <- getDownloader forgeDir
                tokenDownload d
            case res of
                Left e -> putStrLn $ "main: Error: " ++ e
                Right _ -> putStrLn "Success"
        [] -> help
    pure ()
