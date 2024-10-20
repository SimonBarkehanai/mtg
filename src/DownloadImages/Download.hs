{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DownloadImages.Download (
    download,
    tokenDownload,
    handleForgeLogLine,
    getDownloader,
    Downloader (manager),
) where

import Control.Monad (forM, forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (
    ExceptT (..),
    except,
    runExceptT,
    tryE, -- codespell:ignore trye
 )
import Control.Monad.Trans.State.Lazy (
    State,
    evalStateT,
    execState,
    get,
    modify,
    put,
 )
import Data.ByteString (stripSuffix)
import Data.ByteString as BS (
    ByteString,
    elemIndex,
    elemIndexEnd,
    split,
    splitAt,
    take,
    toFilePath,
    toStrict,
 )
import qualified Data.ByteString as BS (
    concat,
    drop,
    dropWhile,
    isPrefixOf,
    length,
    null,
    readFile,
    takeWhile,
 )
import Data.ByteString.Builder (
    byteString,
    intDec,
    stringUtf8,
    toLazyByteString,
 )
import Data.ByteString.Char8 (pack, readInt, strip, unpack)
import qualified Data.ByteString.Char8 as C8BS (dropWhileEnd, map)
import qualified Data.ByteString.Lazy as LBS (writeFile)
import Data.Char (isDigit, toLower, toUpper)
import Data.List (isPrefixOf)
import Data.Map (Map, empty, insert)
import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Debug.Trace (trace)
import Network.HTTP.Client (Manager, Request (..), Response (..), httpLbs, parseRequest)
import Search.Card (collector_number)
import qualified Search.Card as C (Card (..))
import qualified Search.CardFace as CF (CardFace (..))
import Search.ImageURIs (ImageURIs (..))
import Search.Manager (getManager)
import Search.RelatedCard (RelatedCard (RelatedCard, type_line, uri))
import Search.Search (defaultRequest, search, search')
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getHomeDirectory,
    listDirectory,
 )
import System.Environment (lookupEnv)
import System.FilePath ((</>))

type PicsDir = FilePath

data Dirs = Dirs {dirsCache :: FilePath, dirsPics :: FilePath}

getPicsDir :: FilePath -> ExceptT String IO PicsDir
getPicsDir fDir = do
    f <- lift $ doesDirectoryExist fDir
    p <- lift $ doesFileExist propPath
    if not f
        then
            pure $ fDir ++ " does not exist or is not a directory"
        else
            if not p
                then
                    pure $ propPath ++ "does not exist or is not a file"
                else lift pd
  where
    propPath = fDir </> "forge.profile.properties"
    pd = do
        home <- getHomeDirectory
        c <- lookupEnv "XDG_CACHE_HOME"
        let defCache = fromMaybe (home </> ".cache") c
        props <- Prelude.readFile propPath
        pure $ parsePicsDir defCache props
    parsePicsDir defCache props =
        let Dirs{dirsCache = c, dirsPics = p} =
                execState (mapM_ handleLine $ lines props) $
                    Dirs defCache ""
            p' = if null p then c </> "pics" else p
         in fDir </> p' -- if p' is absolute </> returns p'
    handleLine l
        | "cacheDir=" `isPrefixOf` l = modify (\d -> d{dirsCache = drop 9 l})
        | "cardPicsDir=" `isPrefixOf` l = modify (\d -> d{dirsPics = drop 12 l})
        | otherwise = pure ()

data SetInfo = SetInfo {forge :: ByteString, sf :: ByteString, dir :: ByteString}
    deriving (Show)
defSetInfo :: SetInfo
defSetInfo = SetInfo{forge = "", sf = "", dir = ""}
type SetMap = Map ByteString SetInfo
data Downloader = Downloader
    { forgeDir :: FilePath
    , picsDir :: PicsDir
    , sfMap :: SetMap
    , forgeMap :: SetMap
    , tokens :: [(SetInfo, ByteString)]
    , manager :: Manager
    }
instance Show Downloader where
    show
        Downloader
            { forgeDir = fd
            , picsDir = pd
            , sfMap = s
            , forgeMap = f
            , tokens = t
            } =
            "Downloader {forgeDir="
                ++ show fd
                ++ ", picsDir="
                ++ show pd
                ++ ", sfMap="
                ++ show s
                ++ ", forgeMap="
                ++ show f
                ++ ", tokens="
                ++ show t
                ++ "}"

getDownloader :: FilePath -> ExceptT String IO Downloader
getDownloader fDir = do
    pd <- getPicsDir fDir
    editions <- lift $ map (edDir </>) <$> listDirectory edDir
    contents <- lift $ mapM BS.readFile editions
    infos <- except $ mapM getInfo contents
    m <- lift getManager
    (infos', tkns) <- pure $ foldr combineTokens ([], []) infos
    pure $
        execState
            (mapM_ ins infos')
            Downloader
                { forgeDir = fDir
                , picsDir = pd
                , sfMap = empty
                , tokens = tkns
                , forgeMap = empty
                , manager = m
                }
  where
    res = fDir </> "res"
    edDir = res </> "editions"
    getInfo c =
        let (SetInfo{forge = f, sf = s, dir = d}, tkns, _) =
                execState
                    (mapM_ handleLine $ split 10 c)
                    (defSetInfo, [], False)
            s' = if BS.length s <= 2 then f else s
            d' = if BS.null d then f else d
         in if BS.null f
                then Left "Couldn't find correct set codes"
                else
                    Right
                        ( SetInfo
                            { forge = C8BS.map toUpper f
                            , sf = C8BS.map toUpper s'
                            , dir = d'
                            }
                        , tkns
                        )
      where
        handleLine :: ByteString -> State (SetInfo, [ByteString], Bool) ()
        handleLine l
            | "Code=" `BS.isPrefixOf` l =
                modify
                    (\(i, t, b) -> (i{forge = BS.drop 5 l}, t, b))
            | "Code2=" `BS.isPrefixOf` l =
                modify
                    (\(i, t, b) -> (i{dir = BS.drop 6 l}, t, b))
            | "Alias=" `BS.isPrefixOf` l =
                modify
                    (\(i, t, b) -> (i{sf = BS.drop 6 l}, t, b))
            | "ScryfallCode=" `BS.isPrefixOf` l =
                modify
                    (\(i, t, b) -> (i{sf = BS.drop 13 l}, t, b))
            | "[tokens]" `BS.isPrefixOf` l =
                modify (\(i, t, _) -> (i, t, True))
            | "[" `BS.isPrefixOf` l =
                modify (\(i, t, _) -> (i, t, False))
            | BS.length l /= 0 =
                modify (\(i, t, b) -> (i, if b then l : t else t, b))
            | otherwise = pure ()
    ins i@SetInfo{forge = f, sf = s} = do
        dl <- get
        let sm = sfMap dl
            fm = forgeMap dl
        put dl{sfMap = insert s i sm, forgeMap = insert f i fm}
    combineTokens (i, setTkns) (sets, allTkns) =
        ( i : sets
        , map (i,) setTkns ++ allTkns
        )

download, downloadFile, downloadDir, downloadSearch :: Downloader -> String -> IO ()
download downloader s = do
    file <- doesFileExist s
    d <- doesDirectoryExist s
    if file
        then downloadFile downloader s
        else
            if d
                then
                    downloadDir downloader s
                else
                    downloadSearch downloader s
downloadFile downloader f = do
    content <- BS.readFile f
    mapM_ handleLine $ split newline content
  where
    handleLine l =
        let parts = split pipe l
         in if length parts < 2 then pure () else handleCard parts
    handleCard [n, s] = handleCard [n, s, "1"]
    handleCard [n, s, idx] = downloadCard downloader n' sCode idx
      where
        n' = BS.drop 1 $ BS.dropWhile (/= space) n
        sCode = maybe "ERROR" sf $ Map.lookup s $ forgeMap downloader
    handleCard _ = pure ()
downloadDir downloader d =
    listDirectory d >>= mapM_ (download downloader . (d </>))
downloadSearch downloader@Downloader{manager = m} s = do
    results <- runExceptT $ search m (pack s)
    case results of
        Left e -> putStrLn e
        Right r -> mapM_ (flip (downloadCard' downloader) "") r

data DLInfo = DLInfo {url :: String, setDir :: String, filename :: ByteString}
    deriving (Show)

downloadCard :: Downloader -> ByteString -> ByteString -> ByteString -> IO ()
downloadCard downloader@Downloader{manager = m} n s idx = do
    cards <- runExceptT $ search m q
    case cards of
        Left e -> print q >> putStrLn e
        Right cs -> downloadCard' downloader c idx''
          where
            c = cs !! idx'
            idx'' = if singleton then "" else idx
            singleton =
                -- Foil-Only Booster Cards
                length cs == 1
                    || ( length cs == 2
                            && last (collector_number (cs !! 1)) == '★'
                       )
  where
    q =
        toStrict $
            toLazyByteString $
                "!\""
                    <> byteString n
                    <> "\" s:\""
                    <> byteString s
                    <> "\" unique:prints order:set"
    idx' = case readInt idx of
        Just (x, _) -> x - 1
        Nothing -> 0

downloadCard' :: Downloader -> C.Card -> ByteString -> IO ()
downloadCard' Downloader{picsDir = pd, sfMap = s, manager = m} c idx =
    mapM_
        (downloadImg m $ pd </> "cards")
        (dlInfo c idx s)

face :: Int -> C.Card -> Maybe CF.CardFace
face i C.Card{C.card_faces = f} = fmap (!! i) f

dlInfo :: C.Card -> ByteString -> SetMap -> [DLInfo]
dlInfo c@C.Card{C.layout = l} idx sm
    | l
        `elem` [ "normal"
               , "leveler"
               , "saga"
               , "planar"
               , "scheme"
               , "vanguard"
               , "token"
               , "emblem"
               , "augment"
               , "host"
               , "art_series"
               , "class"
               , "prototype"
               ] =
        [ DLInfo
            { url = png imgs
            , setDir = d
            , filename =
                toStrict $
                    toLazyByteString $
                        stringUtf8 (C.name c)
                            <> byteString idx
                            <> ".full.png"
            }
        ]
    | l
        `elem` [ "transform"
               , "double_faced_token"
               , "modal_dfc"
               , "double_sided"
               ] =
        [ DLInfo
            { url = maybe (error "no images") png $ CF.image_uris front
            , setDir = d
            , filename =
                toStrict $
                    toLazyByteString $
                        stringUtf8 (CF.name front)
                            <> byteString idx
                            <> ".full.png"
            }
        , DLInfo
            { url = maybe (error "no images") png $ CF.image_uris back
            , setDir = d
            , filename =
                toStrict $
                    toLazyByteString $
                        stringUtf8 (CF.name back)
                            <> byteString idx
                            <> ".full.png"
            }
        ]
    | l `elem` ["flip", "adventure"] =
        [ DLInfo
            { url = png imgs
            , setDir = d
            , filename =
                toStrict $
                    toLazyByteString $
                        stringUtf8 (CF.name front)
                            <> byteString idx
                            <> ".full.png"
            }
        ]
    | l == "meld" =
        [ DLInfo
            { url = png imgs
            , setDir = d
            , filename =
                toStrict $
                    toLazyByteString $
                        stringUtf8 (C.name c)
                            <> byteString idx
                            <> ".full.png"
            }
            -- TODO: Also download meld result
        ]
    | l == "split" =
        [ DLInfo
            { url = png imgs
            , setDir = d
            , filename =
                toStrict $
                    toLazyByteString $
                        stringUtf8 (CF.name front)
                            <> stringUtf8 (CF.name back)
                            <> byteString idx
                            <> ".full.png"
            }
        ]
    | otherwise = error $ "unknown layout " ++ l
  where
    front = fromMaybe (error "no faces") $ face 0 c
    back = fromMaybe (error "no faces") $ face 1 c
    imgs = fromMaybe (error "no images") $ C.image_uris c
    set = map toUpper $ C.set c
    d = unpack $ dir $ fromMaybe defSetInfo $ Map.lookup (pack set) sm

downloadImg :: Manager -> PicsDir -> DLInfo -> IO ()
downloadImg m pd DLInfo{url = u, setDir = s, filename = n} = do
    req <- parseRequest u
    res <- httpLbs req m
    n' <- toFilePath n
    let d = pd </> s'
    createDirectoryIfMissing True d
    LBS.writeFile (d </> n') $ responseBody res
  where
    s' = map toUpper s

newline, space, underscore, pipe :: Word8
newline = 10
space = 32
underscore = 95
pipe = 124

tokenDownload :: Downloader -> ExceptT String IO ()
tokenDownload
    Downloader
        { forgeDir = fDir
        , picsDir = pd
        , manager = m
        , tokens = tkns
        } =
        mapM (uncurry $ tokenInfo m tokenscripts) tkns
            >>= (\x -> lift (print x) >> pure x)
            >>= (lift . mapM_ (downloadTok m $ pd </> "tokens")) . concat
      where
        tokenscripts = fDir </> "res" </> "tokenscripts"

tokenInfo :: Manager -> FilePath -> SetInfo -> ByteString -> ExceptT String IO [DLInfo]
tokenInfo m tokenscripts SetInfo{forge = f, sf = s, dir = d} fname = do
    fname' <- lift $ toFilePath fname
    let file = tokenscripts </> fname' ++ ".txt"
    exists <- lift $ doesFileExist file
    if not exists
        then lift (putStrLn $ file ++ " does not exist") >> pure []
        else do
            contents <- lift $ readFile file
            let name = execState (mapM_ storeName $ lines contents) ""
                q = "!\"" <> name <> "\" include:extras t:token"
                q' = q <> " unique:prints s:T" <> s
            results <-
                tryE (search m q') -- codespell:ignore trye
                    >>= either
                        ( const $
                            tryE (search m q) -- codespell:ignore trye
                                >>= either
                                    ( \e' ->
                                        lift (putStrLn e')
                                            >> pure []
                                    )
                                    pure
                        )
                        pure
            except $ concat <$> evalStateT (mapM info results) 0
  where
    storeName l
        | "Name:" `isPrefixOf` l =
            put $ removeSuffix " Token" $ strip $ BS.drop 5 $ pack l
        | otherwise = pure ()
    info c@C.Card{C.name = n, C.layout = l} = do
        i <- get
        put $ succ i
        let idx = if i == 0 then "" else intDec $ succ i
            builder =
                byteString fname
                    <> idx
                    <> "_"
                    <> byteString (C8BS.map toLower f)
                    <> ".png"
        case l of
            "token" ->
                pure
                    [ DLInfo
                        { url =
                            png
                                $ fromMaybe
                                    (error $ "no images: " ++ show c)
                                $ C.image_uris c
                        , setDir = unpack d
                        , filename = toStrict $ toLazyByteString builder
                        }
                    ]
            "double_faced_token" ->
                pure
                    [ DLInfo
                        { url =
                            png $
                                fromMaybe (error $ "no images: " ++ show c) $
                                    CF.image_uris front
                        , setDir = unpack d
                        , filename =
                            toStrict $
                                toLazyByteString $
                                    byteString fname
                                        <> idx
                                        <> "_"
                                        <> byteString (C8BS.map toLower f)
                                        <> ".png"
                        }
                    , DLInfo
                        { url =
                            png $
                                fromMaybe (error $ "no images: " ++ show c) $
                                    CF.image_uris back
                        , setDir = unpack d
                        , filename =
                            toStrict $
                                toLazyByteString $
                                    byteString fname
                                        <> idx
                                        <> "_"
                                        <> byteString (C8BS.map toLower f)
                                        <> ".png"
                        }
                    ]
            "flip" -> trace ("Handle flip layout (" ++ n ++ ")") $ pure []
            _ -> lift $ Left $ "unknown layout: " ++ l ++ " (" ++ show c ++ ")"
      where
        front = fromMaybe (error "no faces") $ face 0 c
        back = fromMaybe (error "no faces") $ face 1 c
    removeSuffix suf x = fromMaybe x $ stripSuffix suf x

downloadTok :: Manager -> PicsDir -> DLInfo -> IO ()
downloadTok m pd DLInfo{url = u, filename = n} = do
    req <- parseRequest u
    res <- httpLbs req m
    n' <- toFilePath n
    createDirectoryIfMissing True pd
    putStrLn $ "writing to " ++ pd </> n'
    LBS.writeFile (pd </> n') $ responseBody res

handleForgeLogLine :: Downloader -> String -> ExceptT String IO ()
handleForgeLogLine
    d@Downloader
        { forgeDir = fd
        , picsDir = pd
        , manager = m
        , forgeMap = fm
        }
    s
        | "Fetch due to missing key: " `isPrefixOf` s = handleMissing s'
        | otherwise = pure ()
      where
        s' = drop 26 s
        handleMissing ('c' : ':' : c) =
            maybe
                (pure ())
                (lift . uncurry3 (downloadCard d))
                parts
          where
            c' = pack c
            end =
                BS.elemIndex pipe c'
                    >>= \p -> (p +) <$> BS.elemIndex space (BS.drop p c')
            key = flip BS.take c' <$> end
            parts = case split pipe <$> key of
                Just [n, set] -> Just (n, set, "1")
                Just [n, set, i] -> Just (n, set, i)
                _ -> Nothing
            uncurry3 f (x, y, z) = f x y z
        handleMissing ('t' : ':' : t)
            | "emblem_" `isPrefixOf` t = do
                cards <- search m emblemSearch
                emblems <-
                    concat
                        <$> forM
                            cards
                            ( \C.Card{C.all_parts = p} ->
                                let u = foldr (\RelatedCard{type_line = tl, uri = new} old -> if "Emblem" `isPrefixOf` tl then Just $ pack new else old) Nothing =<< p
                                 in maybe (pure []) (\u' -> search' m defaultRequest{path = BS.drop 24 u'}) u
                            )
                forM_ emblems (lift . downloadTok m (pd </> "tokens") . emblemInfo)
            | otherwise =
                maybe
                    (pure ())
                    (lift . mapM_ (downloadTok m $ pd </> "tokens") =<<)
                    info
          where
            t' = pack t
            pos = elemIndexEnd underscore t'
            (fname, set) = functorUnzip (flip BS.splitAt t' <$> pos)
            fname' = C8BS.dropWhileEnd isDigit <$> fname
            set' = BS.takeWhile (/= space) . BS.drop 1 <$> set
            si = flip Map.lookup fm . C8BS.map toUpper =<< set'
            info = tokenInfo m tokenscripts <$> si <*> fname'

            emblemName = BS.takeWhile (/= space) $ BS.drop 7 t'
            emblemParts = BS.split underscore emblemName
            emblemMaybeSet = Map.lookup (C8BS.map toUpper $ last emblemParts) fm
            emblemSearch =
                toStrict $
                    toLazyByteString $
                        maybe
                            ("!\"" <> byteString (BS.concat emblemParts) <> "\"")
                            ( \st ->
                                "!\""
                                    <> byteString (BS.concat $ init emblemParts)
                                    <> "\" s:\""
                                    <> byteString (sf st)
                                    <> "\""
                            )
                            emblemMaybeSet
            emblemInfo c@C.Card{} =
                DLInfo
                    { url =
                        png
                            $ fromMaybe
                                (error $ "no images: " ++ show c)
                            $ C.image_uris c
                    , setDir = ""
                    , filename =
                        toStrict $
                            toLazyByteString $
                            "emblem_"<>
                                byteString emblemName
                                    <> ".png"
                    }

            functorUnzip xs = (fst <$> xs, snd <$> xs)
            tokenscripts = fd </> "res" </> "tokenscripts"
        handleMissing _ = pure ()
