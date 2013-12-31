{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Aeson.Types
import Control.Applicative ((<$>), (<*>), (<|>), pure, empty)
import Control.Monad as M
import qualified Data.ByteString.Lazy as BL
import System.Directory
import System.Random

data Token = PreEntry | Entry T.Text | PostEntry
    deriving (Show, Read, Eq, Ord)

type TokenMap a = M.Map (V.Vector Token) a
type RawDatabase = TokenMap (TokenMap Int)
type TTokenMap = TokenMap (TVar (TokenMap Int))
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
    deriving (Show, Read, Eq, Ord)

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
instance FromJSON Token where
    parseJSON v = case v of
        Bool b -> case b of
            False -> pure PreEntry
            True -> pure PostEntry
        String t -> pure $ Entry t
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


tokenize :: Int -> T.Text -> [V.Vector Token]
tokenize size text = iterateTokens size (-1) $ V.fromList $ T.words text

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
                e <- newTVar $ M.singleton s 1
                writeTVar db $ M.insert f e d
            Just x -> do
                v <- readTVar x
                writeTVar x $ M.alter a s v

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
    return m'

toRawContext :: Context -> IO RawContext
toRawContext c = do
    db <- convertToRaw $ dbContext c
    return $ RawContext db (vecSize c) (startVec c)

fromRawContext :: RawContext -> IO Context
fromRawContext rc = do
    m' <- T.mapM newTVarIO $ dbRawContext rc
    m <- newTVarIO m'
    return $ Context m (vecRawSize rc) (startRawVec rc)

loadDatabase :: ContextMap -> FilePath -> IO ()
loadDatabase contexts fp = do
    exists <- doesFileExist fp
    when exists $ do
        f <- BL.readFile fp
        putStrLn "Loading Database"
        case eitherDecode f of
            Left err -> do
                putStrLn "Database Load Failed!"
                putStrLn err
            Right v -> do
                c <- T.mapM fromRawContext v
                atomically $ writeTVar contexts c
                putStrLn "Loaded Database"

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
            u <- readTVar v
            let total = M.foldr (+) 0 u
                (s, r') = randomR (0, total-1) r
                (_, Just res) = M.foldrWithKey f (s, Nothing) u
            return (res, r')
        case V.unsafeLast res of
            Entry t -> do
                rest <- markovGen r' c res
                return $ V.cons t rest
            _       -> return V.empty

    where
        allEnds = V.and $ V.map (==PostEntry) ts
        f _ _ (a, Just x)   = (a, Just x)
        f k v (a, Nothing)  = if a - v <= 0
            then (0, Just k)
            else (a - v, Nothing)


main :: IO ()
main = do
    putStrLn "Starting server"
    let fp = "db.json"
    contexts <- newTVarIO M.empty
    loadDatabase contexts fp

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
        get "/dump/:name" $ do
            name <- param "name"
            cs <- liftIO $ readTVarIO contexts
            let (Just v) = M.lookup name cs
            raw <- liftIO $ convertToRaw $ dbContext v
            json raw
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
        post "/save" $ liftIO $ do
            cs <- readTVarIO contexts
            m <- T.mapM toRawContext cs
            BL.writeFile fp $ encode m
        post "/reloaddb" $ liftIO $ loadDatabase contexts fp
        get "/gen/:name" $ do
            name <- param "name"
            cs <- liftIO $ readTVarIO contexts
            let (Just v) = M.lookup name cs
                start    = startVec v
            gen <- liftIO $ newStdGen
            res <- liftIO $ markovGen gen v start
            text $ TL.fromStrict $ T.intercalate " " $ V.toList res






