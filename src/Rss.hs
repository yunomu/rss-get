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
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import Data.XML.Types (Event(..))
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML.Stream.Parse as XML
import qualified Text.XML.Stream.Parse.Util as XML

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

itemConduit :: MonadThrow m => Conduit ByteString m RssItem
itemConduit = XML.parseBytes XML.def =$= c
  where
    c = await >>= maybe (return ()) (\a -> yield (RssItem a) >> c)

getRss :: String -> IO ()
getRss uri = do
    feed <- getFeed uri
    runResourceT $ do
        feed $=+ itemConduit >>= ($$+- sinkStdout)

test :: IO ()
test = runResourceT $
    CB.sourceFile "test/dump.txt"
    $= itemConduit
    $$ sinkStdout
