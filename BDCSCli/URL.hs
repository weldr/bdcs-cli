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

module BDCSCli.URL(apiUrl,
                   deleteUrl,
                   getUrl,
                   postUrl)
  where

import           Control.Lens ((&), (.~))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List (intercalate)
import           Network.Wreq.Session as S
import           Network.Wreq

import           BDCSCli.Cmdline(CliOptions(..))
import           BDCSCli.Utilities(maybeIO)

-- | Construct an API URL based on cmdline options or defaults.
apiUrl :: CliOptions -> String -> String
apiUrl CliOptions{..} route = intercalate "/" [optUrl, "api/v" ++ optApi, route]

-- | Fetch data from a URL and ignore errors by returning Nothing, or a Lazy ByteString
getUrl :: Session -> String -> IO (Maybe (Response BSL.ByteString))
getUrl sess url = maybeIO (S.get sess url)

-- | Post a String to a URL and return the Response from the server, or Nothing
postUrl :: Session -> String -> String -> IO (Maybe (Response BSL.ByteString))
postUrl sess url bodyStr = do
    let opts = defaults & header "Content-Type" .~ ["text/x-toml"]
    let bodyBytes = C8.pack bodyStr
    maybeIO (S.postWith opts sess url bodyBytes)

-- | Send a DELETE request to the server and return the Response, or Nothing
deleteUrl :: Session -> String -> IO (Maybe (Response BSL.ByteString))
deleteUrl sess url = maybeIO (S.delete sess url)
