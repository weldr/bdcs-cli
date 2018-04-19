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

module BDCSCli.API.Types.ComposeInfo(
    ComposeInfoResponse(..),
    decodeComposeInfoResponse
) where

import           Control.Lens((^.))
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C8
import           Network.Wreq(Response, responseBody)

import           BDCSCli.API.Types.Recipe(Recipe(..))

data ComposeInfoResponse = ComposeInfoResponse {
    cirCommit      :: T.Text,                                           -- ^ Blueprint git commit hash
    cirBlueprint   :: Recipe,                                           -- ^ Frozen Blueprint
    cirType        :: T.Text,                                           -- ^ Build type (tar, etc.)
    cirBuildId     :: T.Text,                                           -- ^ Build UUID
    cirQueueStatus :: T.Text                                            -- ^ Build queue status
} deriving (Show, Eq)

instance ToJSON ComposeInfoResponse where
  toJSON ComposeInfoResponse{..} = object
    [ "commit"       .= cirCommit
    , "blueprint"    .= cirBlueprint
    , "compose_type" .= cirType
    , "id"           .= cirBuildId
    , "queue_status" .= cirQueueStatus
    ]

instance FromJSON ComposeInfoResponse where
  parseJSON = withObject "/compose/info response" $ \o -> do
    cirCommit      <- o .: "commit"
    cirBlueprint   <- o .: "blueprint"
    cirType        <- o .: "compose_type"
    cirBuildId     <- o .: "id"
    cirQueueStatus <- o .: "queue_status"
    return ComposeInfoResponse{..}

-- | Convert the server response into data structures
decodeComposeInfoResponse :: Response C8.ByteString -> Maybe ComposeInfoResponse
decodeComposeInfoResponse resp = decode $ resp ^. responseBody
