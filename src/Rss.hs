module Rss
    ( getRss
    ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Internal as CI
import Data.Text (Text)
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML.Stream.Parse as XML
import System.IO (stdout)

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
        liftIO $ putStr $ show a
        sinkStdout
    )

getRss :: String -> IO ()
getRss uri = do
    feed <- getFeed uri
    runResourceT $ do
        events <- feed $=+ XML.parseBytes XML.def
        events $$+- sinkStdout
