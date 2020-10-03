{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.FeatureFlags
  ( Store (..),
    Flags,
    fetch,
    memoryStore,
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as Map
import qualified Data.IORef as IORef
import Data.Kind (Type)
import qualified Data.Maybe as Maybe
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as Read
import qualified Data.Word as Word
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, TypeError, symbolVal)
import qualified Network.Wai as Wai
import qualified Paths_wai_feature_flags as Paths
import System.Random (StdGen, getStdRandom, randomR)

application :: Flags flags => Proxy flags -> Store -> Wai.Application
application flagsType store req respond = do
  frontend <- Paths.getDataFileName "frontend/index.html"
  case (Wai.requestMethod req, Wai.pathInfo req) of
    ("GET", []) ->
      respond $
        Wai.responseFile
          (toEnum 200)
          [("Content-Type", "text/html; charset=UTF-8")]
          frontend
          Nothing
    ("GET", ["flags"]) -> do
      states <- state (flags flagsType) store
      respond $
        Wai.responseLBS
          (toEnum 200)
          [("Content-Type", "application/json")]
          (Aeson.encode states)
    ("PUT", ["flags", flagName]) -> do
      body <- Wai.lazyRequestBody req
      case Aeson.decode body of
        Nothing ->
          respond (Wai.responseLBS (toEnum 400) [] "")
        Just percent -> do
          update flagName percent store
          respond (Wai.responseLBS (toEnum 200) [] "")
    _ -> respond (Wai.responseLBS (toEnum 404) [] "")

data Store
  = Store
      { readKeys :: IO [(B.ByteString, B.ByteString)],
        writeKey :: B.ByteString -> B.ByteString -> IO ()
      }

memoryStore :: IO Store
memoryStore = do
  ref <- IORef.newIORef Map.empty
  pure
    Store
      { readKeys = Map.toList <$> IORef.readIORef ref,
        writeKey = \key value ->
          IORef.atomicModifyIORef' ref (\xs -> (Map.insert key value xs, ()))
      }

fetch :: forall flags. Flags flags => Store -> IO flags
fetch store = do
  let keys = flags (Proxy :: Proxy flags)
  states <- state keys store
  getStdRandom (generate states)

newtype Percent = Percent Word.Word deriving (Aeson.ToJSON, Aeson.FromJSON)

percent :: Word.Word -> Percent
percent = Percent . min 100

class Flags flags where
  generate :: Map.HashMap T.Text Percent -> StdGen -> (flags, StdGen)

  flags :: Proxy flags -> [T.Text]

  default generate :: (Generic flags, GFlags (Rep flags)) => Map.HashMap T.Text Percent -> StdGen -> (flags, StdGen)
  generate states gen = first GHC.Generics.to $ ggenerate states gen

  default flags :: (Generic flags, GFlags (Rep flags)) => Proxy flags -> [T.Text]
  flags _ = gflags (Proxy :: Proxy (Rep flags))

class GFlags flags where
  ggenerate :: Map.HashMap T.Text Percent -> StdGen -> (flags g, StdGen)

  gflags :: Proxy flags -> [T.Text]

instance GFlags fields => GFlags (D1 m (C1 ('MetaCons s f 'True) fields)) where
  ggenerate states gen = first (M1 . M1) $ ggenerate states gen

  gflags _ = gflags (Proxy :: Proxy fields)

instance (GFlags l, GFlags r) => GFlags (l :*: r) where
  ggenerate states gen =
    let (lval, gen') = ggenerate states gen
        (rval, gen'') = ggenerate states gen'
     in (lval :*: rval, gen'')

  gflags _ = gflags (Proxy :: Proxy l) <> gflags (Proxy :: Proxy r)

instance
  ( KnownSymbol fieldName,
    FromBool (IsBool bool) bool
  ) =>
  GFlags (S1 ('MetaSel ('Just fieldName) su ss ds) (K1 i bool))
  where
  ggenerate states gen =
    first (M1 . K1 . fromBool (Proxy :: Proxy (IsBool bool))) $
      case Map.lookup (T.pack $ symbolVal (Proxy :: Proxy fieldName)) states of
        Nothing -> (False, gen)
        Just (Percent 0) -> (False, gen)
        Just (Percent 100) -> (True, gen)
        Just state -> roll gen state

  gflags _ = [T.pack $ symbolVal (Proxy :: Proxy fieldName)]

type family IsBool (b :: Type) :: Bool where
  IsBool Bool = 'True
  IsBool _ = 'False

class FromBool (b :: Bool) a where
  fromBool :: Proxy b -> Bool -> a

instance FromBool 'True Bool where
  fromBool _ = id

instance TypeError InvalidFlagsTypeMessage => FromBool 'False a where
  fromBool = error "unreachable"

instance TypeError InvalidFlagsTypeMessage => GFlags (D1 m (C1 ('MetaCons s f 'False) a)) where
  ggenerate = error "unreachable"

  gflags = error "unreachable"

type InvalidFlagsTypeMessage =
  'Text "Not a valid flags type."
    :$$: 'Text "A flags type needs to be a record with boolean fields."
    :$$: 'Text "For example:"
    :$$: 'Text "  data Flags ="
    :$$: 'Text "     Flags { showErrorPage    :: Bool"
    :$$: 'Text "           , throttleRequests :: Bool }"

roll :: StdGen -> Percent -> (Bool, StdGen)
roll gen (Percent trueChance) =
  let (randomPercentage, gen') = randomR (1, 100) gen
   in (trueChance >= randomPercentage, gen')

update :: T.Text -> Percent -> Store -> IO ()
update flag (Percent percentage) store =
  writeKey
    store
    (TE.encodeUtf8 flag)
    (TE.encodeUtf8 (T.pack (show (min 100 percentage))))

state :: [T.Text] -> Store -> IO (Map.HashMap T.Text Percent)
state keys store = do
  let defaults = zip keys (repeat (Percent 0))
  stored <- Maybe.catMaybes . map decodeFlag <$> readKeys store
  pure $ Map.fromList $ stored <> defaults

decodeFlag :: (B.ByteString, B.ByteString) -> Maybe (T.Text, Percent)
decodeFlag (key, val) = do
  flag <- either (const Nothing) Just $ TE.decodeUtf8' key
  enabledString <- either (const Nothing) Just $ TE.decodeUtf8' val
  (enabledInt, _) <- either (const Nothing) Just $ Read.decimal enabledString
  pure (flag, percent enabledInt)
