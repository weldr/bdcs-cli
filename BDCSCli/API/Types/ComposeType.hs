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

module BDCSCli.API.Types.ComposeType(
    decodeComposeTypesResponse,

    ComposeType(..),
    ComposeTypesResponse(..)
) where

import           Control.Lens((^.))
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T
import           Network.Wreq(Response, responseBody)

-- | The JSON response for /compose/types
data ComposeType = ComposeType {
    ctEnabled :: Bool,                      -- ^ Is this output type enabled?
    ctName    :: T.Text                     -- ^ The name of the output type
} deriving (Show, Eq)

instance ToJSON ComposeType where
    toJSON ComposeType{..} = object
      [ "enabled" .= ctEnabled
      , "name"    .= ctName
      ]

instance FromJSON ComposeType where
    parseJSON = withObject "compose type" $ \o -> do
        ctEnabled <- o .: "enabled"
        ctName    <- o .: "name"
        return ComposeType{..}

data ComposeTypesResponse = ComposeTypesResponse {
    ctrTypes :: [ComposeType]
} deriving (Show, Eq)

instance ToJSON ComposeTypesResponse where
  toJSON ComposeTypesResponse{..} = object
      [ "types" .= ctrTypes
      ]

instance FromJSON ComposeTypesResponse where
  parseJSON = withObject "/compose/types response" $ \o -> do
      ctrTypes <- o .: "types"
      return ComposeTypesResponse{..}

decodeComposeTypesResponse :: Response C8.ByteString -> Maybe ComposeTypesResponse
decodeComposeTypesResponse resp = decode $ resp ^. responseBody
