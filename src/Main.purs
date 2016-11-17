module Main where

import Prelude
import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut
import Data.Either
import Data.Traversable
import Network.HTTP.Affjax (AJAX, get)

---------------------------------------------------------------------------------

newtype Asset = Asset
  { assetId   :: String
  , mimetype  :: String
  , name      :: String
  , endDate   :: String
  , isEnabled :: Number
  , nocache   :: String
  , isActive  :: Boolean
  , uri       :: String
  , duration  :: String
  , playOrder :: String
  , startDate :: String
  }

---------------------------------------------------------------------------------

instance decodeJsonAsset :: DecodeJson Asset where
  decodeJson json = do
    obj <- decodeJson json

    assetId   <- obj .? "asset_id"
    mimetype  <- obj .? "mimetype"
    name      <- obj .? "name"
    endDate   <- obj .? "end_date"
    isEnabled <- case (obj .? "is_enabled") of
      Left  e -> Right 0.0
      n -> (id n)
    nocache   <- obj .? "nocache"
    isActive  <- obj .? "is_active"
    uri       <- obj .? "uri"
    duration  <- obj .? "duration"
    playOrder <- obj .? "asset_id"
    startDate <- obj .? "start_date"

    pure $ Asset { assetId, mimetype, name, endDate, isEnabled, nocache
                 , isActive, uri, duration, playOrder, startDate
                 }

---------------------------------------------------------------------------------

instance encodeJsonAsset :: EncodeJson Asset where
  encodeJson (Asset a)
    = "asset_id"   := a.assetId
   ~> "mimetype"   := a.mimetype
   ~> "name"       := a.name
   ~> "end_date"   := a.endDate
   ~> "is_enabled" := a.isEnabled
   ~> "nocache"    := a.nocache
   ~> "is_active"  := a.isActive
   ~> "uri"        := a.uri
   ~> "duration"   := a.duration
   ~> "play_order" := a.playOrder
   ~> "start_date" := a.startDate

   ~> jsonEmptyObject

---------------------------------------------------------------------------------

instance showAsset :: Show Asset where
  show (Asset a) =
       "Asset {"
    <> "asset_id: "   <> show a.assetId
    <> "mimetype: "   <> show a.mimetype
    <> "name: "       <> show a.name
    <> "end_date: "   <> show a.endDate
    <> "is_enabled: " <> show a.isEnabled
    <> "end_date: "   <> show a.endDate
    <> "nocache: "    <> show a.nocache
    <> "is_active: "  <> show a.nocache
    <> "uri: "        <> show a.nocache
    <> "duration: "   <> show a.nocache
    <> "play_order: " <> show a.nocache
    <> "start_date: " <> show a.nocache
    <> "}"

---------------------------------------------------------------------------------

main :: forall eff.
        Eff
          ( err     :: EXCEPTION
          , ajax    :: AJAX
          , console :: CONSOLE
          | eff
          )
        (Canceler
          ( ajax    :: AJAX
          , console :: CONSOLE
          | eff
          ))
main = launchAff $ do
  r <- get "http://localhost:8080/api/assets"
  liftEff $ log r.response
  let assets = decodeAssetArray r.response
  case assets of
    Left  e -> liftEff $ log e
    Right r -> liftEff $ log (show r)

decodeAssetArray :: String -> Either String (Array Asset)
decodeAssetArray str = case (jsonParser str) of
  Left  e -> Left e
  Right j -> decodeJson j >>= traverse decodeJson
