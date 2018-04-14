-- Copyright (C) 2018 Red Hat, Inc.
--
-- This file is part of bdcs-cli.
--
-- bdcs-cli is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- bdcs-cli is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with bdcs-cli.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module BDCSCli.API.Types.ComposeStatus(
    ComposeFailedResponse(..),
    ComposeFinishedResponse(..),
    ComposeQueueResponse(..),
    ComposeStatus(..),
    ComposeStatusResponse(..),

    composeStatusText,
    decodeComposeFailedResponse,
    decodeComposeFinishedResponse,
    decodeComposeQueueResponse,
    decodeComposeStatusResponse
) where

import           Control.Lens((^.))
import           Data.Aeson
import           Data.String.ToString
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Time.Clock(UTCTime)
import           Data.Time.Format(defaultTimeLocale, formatTime)
import           Network.Wreq(Response, responseBody)
import           Text.Printf

import           BDCSCli.API.Types.QueueStatus(QueueStatus(..))

data ComposeStatus = ComposeStatus {
    csBuildId       :: T.Text,
    csName          :: T.Text,
    csQueueStatus   :: QueueStatus,
    csTimestamp     :: UTCTime,
    csVersion       :: T.Text
} deriving (Show, Eq)

instance ToJSON ComposeStatus where
    toJSON ComposeStatus{..} = object [
        "id"            .= csBuildId
      , "blueprint"     .= csName
      , "queue_status"  .= csQueueStatus
      , "timestamp"     .= csTimestamp
      , "version"       .= csVersion ]

instance FromJSON ComposeStatus where
    parseJSON = withObject "compose type" $ \o ->
        ComposeStatus <$> o .: "id"
                      <*> o .: "blueprint"
                      <*> o .: "queue_status"
                      <*> o .: "timestamp"
                      <*> o .: "version"

instance ToString ComposeStatus where
    toString c = composeStatusText c

instance PrintfArg ComposeStatus where
    formatArg x fmt | fmtChar (vFmt 's' fmt) == 's' =
        formatString (composeStatusText x) (fmt { fmtChar = 's', fmtPrecision = Nothing })
    formatArg _ fmt = errorBadFormat $ fmtChar fmt

composeStatusText :: ComposeStatus -> String
composeStatusText ComposeStatus{..} = printf "%s %-8s %-15s %s %s\n" csBuildId csQueueStatus csName csVersion timestamp
  where
    timestamp = formatTime defaultTimeLocale "%c" csTimestamp

data ComposeStatusResponse = ComposeStatusResponse {
    csrUuids :: [ComposeStatus]
} deriving (Show, Eq)

instance ToJSON ComposeStatusResponse where
  toJSON ComposeStatusResponse{..} = object [
      "uuids" .= csrUuids ]

instance FromJSON ComposeStatusResponse where
  parseJSON = withObject "/compose/queue/status response" $ \o ->
      ComposeStatusResponse <$> o .: "uuids"

-- | Convert the server response into data structures
decodeComposeStatusResponse :: Response C8.ByteString -> Maybe ComposeStatusResponse
decodeComposeStatusResponse resp = decode $ resp ^. responseBody

data ComposeQueueResponse = ComposeQueueResponse {
    cqrNew :: [ComposeStatus],
    cqrRun :: [ComposeStatus]
} deriving (Show, Eq)

instance ToJSON ComposeQueueResponse where
  toJSON ComposeQueueResponse{..} = object [
      "new" .= cqrNew
    , "run" .= cqrRun ]

instance FromJSON ComposeQueueResponse where
  parseJSON = withObject "/compose/queue response" $ \o ->
      ComposeQueueResponse <$> o .: "new"
                           <*> o .: "run"

-- | Convert the server response into data structures
decodeComposeQueueResponse :: Response C8.ByteString -> Maybe ComposeQueueResponse
decodeComposeQueueResponse resp = decode $ resp ^. responseBody

data ComposeFinishedResponse = ComposeFinishedResponse {
    cfrFinished :: [ComposeStatus]
} deriving (Show, Eq)

instance ToJSON ComposeFinishedResponse where
  toJSON ComposeFinishedResponse{..} = object [
      "finished" .= cfrFinished ]

instance FromJSON ComposeFinishedResponse where
  parseJSON = withObject "/compose/queue/finished response" $ \o ->
      ComposeFinishedResponse <$> o .: "finished"

-- | Convert the server response into data structures
decodeComposeFinishedResponse :: Response C8.ByteString -> Maybe ComposeFinishedResponse
decodeComposeFinishedResponse resp = decode $ resp ^. responseBody

data ComposeFailedResponse = ComposeFailedResponse {
    cfrFailed :: [ComposeStatus]
} deriving (Show, Eq)

instance ToJSON ComposeFailedResponse where
  toJSON ComposeFailedResponse{..} = object [
      "failed" .= cfrFailed ]

instance FromJSON ComposeFailedResponse where
  parseJSON = withObject "/compose/queue/failed response" $ \o ->
      ComposeFailedResponse <$> o .: "failed"

-- | Convert the server response into data structures
decodeComposeFailedResponse :: Response C8.ByteString -> Maybe ComposeFailedResponse
decodeComposeFailedResponse resp = decode $ resp ^. responseBody
