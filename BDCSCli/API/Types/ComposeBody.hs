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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

module BDCSCli.API.Types.ComposeBody(
    decodeComposeResponse,

    ComposeBody(..),
    ComposeResponse(..)
) where

import           Control.Lens((^.))
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Network.Wreq(Response, responseBody)

-- | The JSON POST body for /compose/start
data ComposeBody = ComposeBody {
    cbName :: T.Text,                                                   -- ^ Recipe name (from /blueprints/list)
    cbType :: T.Text,                                                   -- ^ Compose type (from /compose/types)
    cbBranch :: Maybe T.Text                                            -- ^ The git branch to use for this blueprint
} deriving (Show, Eq)

instance ToJSON ComposeBody where
  toJSON ComposeBody{..} = object
    [ "blueprint_name" .= cbName
    , "compose_type"   .= cbType
    , "branch"         .= fromMaybe "master" cbBranch
    ]

instance FromJSON ComposeBody where
    parseJSON = withObject "compose" $ \o -> do
        cbName   <- o .:  "blueprint_name"
        cbType   <- o .:  "compose_type"
        cbBranch <- o .:? "branch"
        return ComposeBody{..}

-- | JSON status response
data ComposeResponse = ComposeResponse {
    crStatus  :: Bool,                                                  -- ^ Success/Failure of the request
    crBuildID :: String                                                 -- ^ UUID of the in-progress build
} deriving (Show, Eq)

instance ToJSON ComposeResponse where
  toJSON ComposeResponse{..} = object
    [ "status"   .= crStatus
    , "build_id" .= crBuildID
    ]

instance FromJSON ComposeResponse where
  parseJSON = withObject "/compose response" $ \o -> do
    crStatus  <- o .: "status"
    crBuildID <- o .: "build_id"
    return ComposeResponse{..}

decodeComposeResponse :: Response C8.ByteString -> Maybe ComposeResponse
decodeComposeResponse resp = decode $ resp ^. responseBody
