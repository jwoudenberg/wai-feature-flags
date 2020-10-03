{-# LANGUAGE DeriveGeneric #-}

module Main
  ( main,
  )
where

import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Network.FeatureFlags as FeatureFlags
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  store <- FeatureFlags.memoryStore
  let app = FeatureFlags.application (Proxy :: Proxy Flags) store
  Warp.run 8080 app

data Flags
  = Flags
      { openWindow :: Bool,
        feedPigeons :: Bool
      }
  deriving (Generic)

instance FeatureFlags.Flags Flags
