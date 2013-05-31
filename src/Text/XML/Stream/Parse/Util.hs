module Text.XML.Stream.Parse.Util
    ( tagPair
    ) where

import Data.Conduit
import qualified Data.Conduit.Internal as CI
import Data.Text (Text)
import Data.XML.Types (Event(..), Name(..))
import qualified Text.XML.Stream.Parse as XML

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

sinkDropWhile :: Monad m => (i -> Bool) -> Consumer i m ()
sinkDropWhile f = whenMaybe await g
  where
    g i | f i       = sinkDropWhile f
        | otherwise = leftover i

isTag :: Event -> Bool
isTag (EventBeginElement _ _) = True
isTag (EventEndElement _)     = True
isTag _                       = False

tagPair :: MonadThrow m
    => Text
    -> CI.ConduitM Event o m a
    -> CI.ConduitM Event o m (Maybe a)
tagPair name inner = do
    sinkDropWhile $ not . isTag
    XML.tagPredicate g (return ()) $ const inner
  where
    g n = nameLocalName n == name
