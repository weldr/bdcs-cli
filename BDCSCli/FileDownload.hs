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
--
-- Based on this blog post:
-- http://www.alfredodinapoli.com/posts/2013-07-20-slick-http-download-in-haskell.html
{-# LANGUAGE OverloadedStrings #-}

module BDCSCli.FileDownload(
    fileWithProgress
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.String.Conversions (cs)
import           Data.List (find, isPrefixOf)
import           Data.List.Split (endBy, splitOn)
import           Data.Maybe (fromMaybe)
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S
import           Network.Http.Client
import           Network.Socket
import           System.FilePath.Posix (takeFileName)
import           Text.Printf (printf)

withProgressBar :: FilePath
                -> InputStream ByteString
                -> OutputStream ByteString
                -> IO ()
withProgressBar filename inS outS = go (0 :: Int)
  where
    go blocksRead = do
      block <- S.read inS
      case block of
        (Just d) -> do
            let currentBlocks = blocksRead + B.length d
            -- TODO Make this show MB and GB as it grows
            printf "%s %10d\r" filename currentBlocks
            S.write (Just d) outS >> go currentBlocks
        Nothing -> do
            putStrLn ""
            return ()

-- | Download a file while showing progress
fileWithProgress :: String -> FilePath -> IO ()
fileWithProgress url default_filename = withSocketsDo $ get (cs url) $ \response inStream ->
    case getStatusCode response of
      200 -> let filename = fromMaybe default_filename (getFilename $ getHeader response "Content-Disposition")
        in S.withFileAsOutput filename (withProgressBar filename inStream)
      -- XXX There is no way to read the body of an error response, readResponseBody is in a hidden part of
      -- the Network.Http.Client library. So just print a generic error for now
      code -> putStrLn $ "ERROR: Failed to download " ++
                      default_filename ++
                      ": http response returned " ++
                      show code

-- | Parse the Content-Disposition header for a filename attachment.
-- Use only the filename, not any path components
getFilename :: Maybe ByteString -> Maybe FilePath
getFilename Nothing = Nothing
getFilename (Just header) =
    case find (isPrefixOf " filename") $ endBy ";" $ cs header of
        Nothing -> Nothing
        Just kv -> case splitOn "=" kv of
            []            -> Nothing
            [_]           -> Nothing
            [_, ""]       -> Nothing
            [_, filename] -> Just $ takeFileName filename
