module Main (main) where

import DownloadImages.Download (download, getDownloader)
import System.Environment (getArgs, getProgName)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class (lift)

help :: IO ()
help = do
    progName <- getProgName
    putStrLn "Usage:"
    putStrLn $ '\t' : progName ++ " <forge directory> <deck/directory/search>..."

main :: IO ()
main = do
    args <- getArgs
    case args of
        (forgeDir : toDownload) -> do
            res <- runExceptT $ do
                d <- getDownloader forgeDir
                lift $ mapM_ (download d) toDownload
            case res of 
                Left e -> putStrLn $ "Error: " ++ e
                Right _ -> putStrLn "Success"
            pure ()
        [] -> help
    pure ()
