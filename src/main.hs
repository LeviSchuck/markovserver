{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Main where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Debug.Trace
import Web.Scotty
import Control.Concurrent.STM
import Data.Traversable as T
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Aeson hiding (json)
import Data.Aeson.Types as AT
import Control.Applicative ((<$>), (<*>), (<|>), pure, empty)
import Control.Monad as M
import qualified Data.ByteString.Lazy as BL
import System.Directory
import System.Random
import Control.Concurrent
import qualified Data.Serialize as S
import GHC.Generics
import Data.Char
import Data.Attoparsec.Number

data Token = PreEntry | Entry T.Text | PostEntry | CharEntry Char
    deriving (Show, Read, Eq, Ord, Generic)

type TokenMap a = M.Map (V.Vector Token) a
type RawDatabase = TokenMap (TokenMap Int)
type TTokenMap = TokenMap (TVar (TokenMap (TVar Int)))
type Database = TVar TTokenMap

data Context = Context
    { dbContext :: Database
    , vecSize   :: Int
    , startVec  :: V.Vector Token
    }

data RawContext = RawContext
    { dbRawContext  :: RawDatabase
    , vecRawSize    :: Int
    , startRawVec   :: V.Vector Token
    }
    deriving (Show, Read, Eq, Ord, Generic)

type ContextMap = TVar (M.Map T.Text Context)

instance (ToJSON a) => ToJSON (TokenMap a) where
    toJSON m = Array ms
        where
            ml = M.toList m
            f ms = object 
                [ ("key", toJSON $ fst ms)
                , ("vals", toJSON $ snd ms)
                ]
            ml1 = map f ml
            ms = V.fromList ml1

instance ToJSON Token where
    toJSON PreEntry = Bool False
    toJSON PostEntry = Bool True
    toJSON (Entry t) = String t
    toJSON (CharEntry c) = AT.Number . I . fromIntegral $ ord c

instance FromJSON Token where
    parseJSON v = case v of
        Bool b -> case b of
            False -> pure PreEntry
            True -> pure PostEntry
        String t -> pure $ Entry t
        AT.Number (I c) -> pure $ CharEntry $ chr $ fromIntegral c
        _ -> typeMismatch "Token" v
instance (FromJSON a) => FromJSON (TokenMap a) where
    parseJSON v = M.fromList <$> parseJSON v
instance (FromJSON a) => FromJSON (V.Vector Token, a) where
    parseJSON (Object o) = (,)
        <$> o .: "key"
        <*> o .: "vals"
    parseJSON _ = mzero

instance ToJSON RawContext where
    toJSON c = object
        [ "db"      .= dbRawContext c
        , "size"    .= vecRawSize c
        , "def"     .= startRawVec c
        ]
instance FromJSON RawContext where
    parseJSON (Object o) = RawContext
        <$> o .: "db"
        <*> o .: "size"
        <*> o .: "def"

instance S.Serialize RawContext where
instance S.Serialize Token where
instance S.Serialize T.Text where
    put t = S.put $ T.encodeUtf8 t 
    get = T.decodeUtf8 <$> S.get
instance (S.Serialize a) => S.Serialize (V.Vector a) where
    put t = S.putListOf S.put $ V.toList t
    get = V.fromList <$> S.getListOf S.get

splitText :: T.Text -> [T.Text]
splitText t
    | T.null t = []
    | otherwise =
        let (a,b) = T.break breakage t
        in if T.null a
            then (T.singleton $ T.head b) : (splitText $ T.tail b)
            else        a : splitText b

breakage :: Char -> Bool
breakage '\'' = False
breakage c
    | isSpace c = True
    | isAlphaNum c = False
    | isPunctuation c = True
    | otherwise = False

tokenize :: Int -> T.Text -> [V.Vector Token]
tokenize size text = iterateTokens size (-1) $ V.fromList $ splitText text

iterateTokens :: Int -> Int -> V.Vector T.Text -> [V.Vector Token]
iterateTokens size pos source
    | pos < size-1  = V.concat [pres, mapEntry part, posts] : iterateTokens size (pos + 1) source
    | mid           = mapEntry part : iterateTokens size (pos + 1) source
    | postcond      = V.concat [mapEntry part, posts] : iterateTokens size (pos + 1) source
    | otherwise     = []
    where
        postpos = size - pos - 1
        pres = V.replicate postpos PreEntry
        posts = V.replicate (max 0 (pos - total + 1)) PostEntry
        mapEntry = V.map Entry
        total = V.length source
        mid = and [postpos <= 0, pos < total]
        postcond = and [pos >= total, pos < total + size]
        todrop = max 0 $ pos + 1 - size
        part = V.drop todrop $ V.take (pos+1) source

addToDatabase :: Database -> [V.Vector Token] -> IO ()
addToDatabase db   []   = return ()
addToDatabase db (_:[])   = return ()
addToDatabase db vts    = do
    atomically $ do
        d <- readTVar db
        case M.lookup f d of
            Nothing -> do
                one <- newTVar 1
                e <- newTVar $ M.singleton s one
                writeTVar db $ M.insert f e d
            Just x -> do
                v <- readTVar x
                -- writeTVar x $ M.alter a s v
                case M.lookup s v of
                    Just c -> do
                        count <- readTVar c
                        writeTVar c $ count + 1
                    Nothing -> do
                        count <- newTVar 1
                        writeTVar x $ M.insert s count v

    addToDatabase db vts'
    where
        f:s:_ = vts
        _:vts' = vts
        a Nothing = Just 1
        a (Just b) = Just $ b + 1

convertToRaw :: Database -> IO RawDatabase
convertToRaw db = do
    m <- readTVarIO db
    m' <- T.mapM readTVarIO m
    m'' <- T.mapM (T.mapM readTVarIO) m'
    return m''

toRawContext :: Context -> IO RawContext
toRawContext c = do
    db <- convertToRaw $ dbContext c
    return $ RawContext db (vecSize c) (startVec c)

fromRawContext :: RawContext -> IO Context
fromRawContext rc = do
    let l = dbRawContext rc
    m1 <- T.mapM (T.mapM newTVarIO) l
    m2 <- T.mapM newTVarIO m1
    m <- newTVarIO m2
    return $ Context m (vecRawSize rc) (startRawVec rc)

saveJSONDatabase :: ContextMap -> FilePath -> IO ()
saveJSONDatabase contexts fp = do
    cs <- readTVarIO contexts
    m <- T.mapM toRawContext cs
    BL.writeFile fp $ encode m

saveBinDatabase :: ContextMap -> FilePath -> IO ()
saveBinDatabase contexts fp = do
    cs <- readTVarIO contexts
    m <- T.mapM toRawContext cs
    BL.writeFile fp $ S.encodeLazy m

loadJSONDatabase :: ContextMap -> FilePath -> IO ()
loadJSONDatabase contexts fp = do
    exists <- doesFileExist fp
    when exists $ do
        f <- BL.readFile fp
        putStrLn "Loading JSON Database"
        case eitherDecode f of
            Left err -> do
                putStrLn "JSON Database Load Failed!"
                putStrLn err
            Right v -> do
                c <- T.mapM fromRawContext v
                atomically $ writeTVar contexts c
                putStrLn "Loaded JSON Database"

loadBinDatabase :: ContextMap -> FilePath -> IO ()
loadBinDatabase contexts fp = do
    exists <- doesFileExist fp
    when exists $ do
        f <- BL.readFile fp
        putStrLn "Loading Binary Database"
        case S.decodeLazy f of
            Left err -> do
                putStrLn "Binary Database Load Failed!"
                putStrLn err
            Right v -> do
                c <- T.mapM fromRawContext v
                atomically $ writeTVar contexts c
                putStrLn "Loaded Binary Database"

markovGen :: StdGen -> Context -> V.Vector Token -> IO (V.Vector T.Text)
markovGen r c ts
    | allEnds = return V.empty
    | otherwise = do  
        -- We aren't going to put all this in a transaction
        -- because the consistent part doesn't matter
        -- so much to me at this time.
        db <- readTVarIO $ dbContext c
        let (Just v) = M.lookup ts db
        (res, r') <- atomically $ do
            -- Unwrap the map
            um <- readTVar v
            u <- T.mapM readTVar um
            let total = M.foldr (+) 0 u
                (s, r') = randomR (0, total-1) r
                (_, Just res) = M.foldrWithKey f (s, Nothing) u
            return (res, r')
        case V.unsafeLast res of
            Entry t -> do
                rest <- markovGen r' c res
                return $ V.cons t rest
            CharEntry c' -> do
                rest <- markovGen r' c res
                return $ V.cons (T.singleton c') rest
            _       -> return V.empty

    where
        allEnds = V.and $ V.map (==PostEntry) ts
        f _ _ (a, Just x)   = (a, Just x)
        f k v (a, Nothing)  = if a - v <= 0
            then (0, Just k)
            else (a - v, Nothing)


main :: IO ()
main = do
    let jsonfp = "db.json"
        binfp = "db.bin"
    contexts <- newTVarIO M.empty
    loadJSONDatabase contexts jsonfp
    loadBinDatabase contexts binfp

    _ <- forkIO $ forever $ do
        threadDelay 600000000
        saveBinDatabase contexts binfp
    putStrLn "Starting server"
    scotty 4800 $ do
        get "/" $ do
            m <- liftIO $ readTVarIO contexts
            let klist = S.toList $ M.keysSet m
            text $ TL.fromStrict $ T.intercalate "\n" klist
        -- This ought to be a post, but for ease right now..
        get "/init/:name/:size" $ do
            name <- param "name"
            size <- param "size"
            liftIO $ atomically $ do
                m <- readTVar contexts
                case M.lookup name m of
                    Just _ -> return ()
                    Nothing -> do
                        m' <- newTVar M.empty
                        let st = V.replicate size PreEntry
                            v = Context m' size st
                        writeTVar contexts $ M.insert name v m
--        get "/dump/:name" $ do
--            name <- param "name"
--            cs <- liftIO $ readTVarIO contexts
--            let (Just v) = M.lookup name cs
--            raw <- liftIO $ convertToRaw $ dbContext v
--            json raw
        get (regex "^/add/([^/]+)/(.*)$") $ do
            name <- param "1"
            entry <- param "2"
            cs <- liftIO $ readTVarIO contexts
            let (Just v) = M.lookup name cs
                tokens = tokenize (vecSize v) entry 
            liftIO $ addToDatabase (dbContext v) tokens
        put "/add/:name" $ do
            name <- param "name"
            b <- body
            let entry' = TL.decodeUtf8 b
                entry = TL.toStrict entry'
            cs <- liftIO $ readTVarIO contexts
            let (Just v) = M.lookup name cs
                tokens = tokenize (vecSize v) entry 
            liftIO $ addToDatabase (dbContext v) tokens
        post "/save" $ do
            liftIO $ saveBinDatabase contexts binfp
        post "/reloaddb" $ liftIO $ do
            loadJSONDatabase contexts jsonfp
            loadBinDatabase contexts binfp
        get "/gen/:name" $ do
            name <- param "name"
            cs <- liftIO $ readTVarIO contexts
            let (Just v) = M.lookup name cs
                start    = startVec v
            gen <- liftIO $ newStdGen
            res <- liftIO $ markovGen gen v start
            text $ TL.fromStrict $ T.concat $ V.toList res






