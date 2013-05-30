{-# LANGUAGE FlexibleContexts #-}

module Rss
    ( getRss
    , test
    ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Internal as CI
import Data.Text (Text)
import Data.XML.Types (Event(..), Name(..))
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML.Stream.Parse as XML

data RssItem = RssItem Event
  deriving Show

getFeed
    :: String -> IO (ResumableSource (ResourceT IO) ByteString)
getFeed uri = runResourceT $ do
    mgr <- liftIO $ HTTP.newManager HTTP.def
    req <- HTTP.parseUrl uri
    HTTP.responseBody <$> HTTP.http req mgr

($=+) :: MonadIO m
    => ResumableSource m a
    -> Conduit a m b
    -> m (ResumableSource m b)
rsrc $=+ cond = do
    (src, fin) <- unwrapResumable rsrc
    return $ CI.ResumableSource (src $= cond) fin

sinkStdout :: (MonadIO m, Show a)
    => Consumer a m ()
sinkStdout = await >>= maybe
    (return ())
    (\a -> do
        liftIO $ putStrLn $ show a
        sinkStdout
    )

isBeginTagName :: Text -> Event -> Bool
isBeginTagName name (EventBeginElement n _)
    | nameLocalName n == name = True
    | otherwise               = False
isBeginTagName _ _ = False

awaitIf :: Monad m
    => (i -> Bool)
    -> Consumer i m (Maybe i)
awaitIf f = await >>= g
  where
    g Nothing       = return Nothing
    g (Just a)
        | f a       = return $ Just a
        | otherwise = awaitIf f

whenMaybe :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenMaybe mma f = mma >>= maybe (return ()) f

--dropWhile 

tagPair :: MonadThrow m
    => Text
    -> CI.ConduitM Event o m a
    -> CI.ConduitM Event o m (Maybe a)
tagPair name inner = do
    undefined

itemConduit :: MonadThrow m => Conduit ByteString m RssItem
itemConduit = XML.parseBytes XML.def =$= c
  where
    c = await >>= maybe (return ()) (\a -> yield (RssItem a) >> c)

getRss :: String -> IO ()
getRss uri = do
    feed <- getFeed uri
    runResourceT $
        feed $=+ itemConduit >>= ($$+- sinkStdout)

test :: IO ()
test = runResourceT $
    CB.sourceFile "test/dump.txt"
    $= itemConduit
    $$ sinkStdout
