module Network.FeatureFlags (Store (..), memoryStore) where

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as Map
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.Proxy as Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as Read
import qualified Data.Word as Word

data Store
  = Store
      { readKeys :: IO [(B.ByteString, B.ByteString)],
        writeKey :: B.ByteString -> B.ByteString -> IO ()
      }

memoryStore :: IO Store
memoryStore = do
  ref <- IORef.newIORef Map.empty
  pure Store
    { readKeys = Map.toList <$> IORef.readIORef ref,
      writeKey = \key value ->
        IORef.atomicModifyIORef' ref (\xs -> (Map.insert key value xs, ()))
    }

data Percent = Percent Word.Word

percent :: Word.Word -> Percent
percent = Percent . min 100

class Flags flags where

  read :: Map.HashMap T.Text Percent -> IO flags

  flags :: Proxy.Proxy flags -> [T.Text]

update :: T.Text -> Percent -> Store -> IO ()
update flag (Percent percentage) store =
  writeKey
    store
    (TE.encodeUtf8 flag)
    (TE.encodeUtf8 (T.pack (show (min 100 percentage))))

state :: Flags flags => Proxy.Proxy flags -> Store -> IO (Map.HashMap T.Text Percent)
state proxy store = do
  let defaults = zip (flags proxy) (repeat (Percent 0))
  stored <- Maybe.catMaybes . map decodeFlag <$> readKeys store
  pure $ Map.fromList $ stored <> defaults

decodeFlag :: (B.ByteString, B.ByteString) -> Maybe (T.Text, Percent)
decodeFlag (key, val) = do
  flag <- either (const Nothing) Just $ TE.decodeUtf8' key
  enabledString <- either (const Nothing) Just $ TE.decodeUtf8' val
  (enabledInt, _) <- either (const Nothing) Just $ Read.decimal enabledString
  pure (flag, percent enabledInt)
