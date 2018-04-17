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

module BDCSCli.API.Types.ComposeDelete(
        ComposeDeleteResponse(..),
        UuidError(..),
        UuidStatus(..),

        decodeComposeDeleteResponse
) where

import           Control.Lens((^.))
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C8
import           Network.Wreq(Response, responseBody)

data UuidError = UuidError {
    ueMsg  :: T.Text,
    ueUuid :: T.Text
} deriving (Show, Eq)

instance ToJSON UuidError where
    toJSON UuidError{..} = object [
        "msg"   .= ueMsg,
        "uuid"  .= ueUuid ]

instance FromJSON UuidError where
    parseJSON = withObject "UUID error type" $ \o ->
        UuidError <$> o .: "msg"
                  <*> o .: "uuid"

data UuidStatus = UuidStatus {
    usStatus :: Bool,
    usUuid :: T.Text
} deriving (Show, Eq)

instance ToJSON UuidStatus where
    toJSON UuidStatus{..} = object [
        "status" .= usStatus,
        "uuid"   .= usUuid ]

instance FromJSON UuidStatus where
    parseJSON = withObject "UUID type" $ \o ->
        UuidStatus <$> o .: "status"
                   <*> o .: "uuid"

data ComposeDeleteResponse = ComposeDeleteResponse {
    cdrErrors :: [UuidError],
    cdrUuids  :: [UuidStatus]
} deriving (Show, Eq)

instance ToJSON ComposeDeleteResponse where
    toJSON ComposeDeleteResponse{..} = object [
        "errors" .= cdrErrors,
        "uuids"  .= cdrUuids ]

instance FromJSON ComposeDeleteResponse where
    parseJSON = withObject "/compose/delete response" $ \o ->
        ComposeDeleteResponse <$> o .: "errors"
                              <*> o .: "uuids"

-- | Convert the server response into data structures
decodeComposeDeleteResponse :: Response C8.ByteString -> Maybe ComposeDeleteResponse
decodeComposeDeleteResponse resp = decode $ resp ^. responseBody

