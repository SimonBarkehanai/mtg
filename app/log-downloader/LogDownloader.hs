module Main (main) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import DownloadImages.Download (getDownloader, handleForgeLogLine)
import System.Environment (getArgs, getProgName)

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
                ls <- lift $ lines <$> getContents
                mapM_ (\s -> lift (putStrLn s) >> handleForgeLogLine d s) ls
            case res of
                Left e -> putStrLn $ "main: Error: " ++ e
                Right _ -> pure ()
        [] -> help
    pure ()
