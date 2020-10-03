{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Foldable (asum)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Network.FeatureFlags as FeatureFlags
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.UrlMap as UrlMap

main :: IO ()
main = do
  store <- FeatureFlags.memoryStore
  let app =
        UrlMap.mapUrls . asum $
          [ UrlMap.mount "flags" (FeatureFlags.application (Proxy :: Proxy Flags) store),
            UrlMap.mountRoot (application store)
          ]
  Warp.run 8080 app

application :: FeatureFlags.Store -> Wai.Application
application store req respond = do
  flags <- FeatureFlags.fetch store
  case (Wai.requestMethod req, Wai.pathInfo req) of
    ("GET", ["window"]) ->
      respond $
        Wai.responseLBS
          (toEnum 200)
          []
          (if openWindow flags then "open" else "closed")
    ("GET", ["pigeons"]) ->
      respond $
        Wai.responseLBS
          (toEnum 200)
          []
          (if feedPigeons flags then "fed" else "hungry")
    _ -> respond (Wai.responseLBS (toEnum 404) [] "")

data Flags
  = Flags
      { openWindow :: Bool,
        feedPigeons :: Bool
      }
  deriving (Generic)

instance FeatureFlags.Flags Flags
