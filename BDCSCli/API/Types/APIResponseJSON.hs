-- Copyright (C) 2017 Red Hat, Inc.
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

module BDCSCli.API.Types.APIResponseJSON(
    APIResponseJSON(..),
    decodeAPIResponse,
) where

import           Control.Lens((^.))
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8
import           Network.Wreq(Response, responseBody)

import           BDCSCli.API.Types.RecipesAPIError

-- | API Status response with possible error messages
data APIResponseJSON = APIResponseJSON
    { arjStatus :: Bool
    , arjErrors :: [RecipesAPIError]
    } deriving Show

instance FromJSON APIResponseJSON where
  parseJSON = withObject "API Response JSON" $ \o -> do
    arjStatus <- o .: "status"
    arjErrors <- o .: "errors"
    return APIResponseJSON{..}

instance ToJSON APIResponseJSON where
  toJSON APIResponseJSON{..} = object
    [ "status" .= arjStatus
    , "errors" .= arjErrors
    ]

-- | Convert the server response into data structures
decodeAPIResponse :: Response C8.ByteString -> Maybe APIResponseJSON
decodeAPIResponse resp = decode $ resp ^. responseBody
