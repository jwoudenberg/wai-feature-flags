module Network.FeatureFlags (Store (..), memoryStore) where

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as Map
import qualified Data.IORef as IORef

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
