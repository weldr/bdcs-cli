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
{-# LANGUAGE RecordWildCards #-}

module BDCSCli.API.Types.RecipeDiff(
    decodeRecipesDiffResponse,
    prettyRecipeDiff,

    RecipeDiffEntry(..),
    RecipesDiffResponse(..),
    RecipeDiffType(..)
) where

import           Control.Lens((^.))
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Foldable(asum)
import           Data.List(sortBy)
import           Data.Maybe(fromJust, fromMaybe)
import           Network.Wreq(Response, responseBody)

import           BDCSCli.API.Types.Recipe

-- | Type of Diff Entry
--
-- Used by RecipeDiffEntry's old and new fields
data RecipeDiffType =
    Name {rdtName :: String}                                    -- ^ Name changed
  | Description {rdtDescription :: String}                      -- ^ Description changed
  | Version {rdtVersion :: Maybe String}                        -- ^ Version changed
  | Module {rdtModule :: RecipeModule}                          -- ^ Module version changed, added, or removed
  | Package {rdtPackage :: RecipeModule}                        -- ^ Package version changed, added, or removed
  | None
  deriving (Show, Eq)

instance FromJSON RecipeDiffType where
  parseJSON = withObject "Recipe diff type" $ \o -> asum
    [ Name        <$> o .: "Name"
    , Description <$> o .: "Description"
    , Version     <$> o .: "Version"
    , Module      <$> o .: "Module"
    , Package     <$> o .: "Package"
    ]

instance ToJSON RecipeDiffType where
  toJSON Name{..}        = object ["Name" .= rdtName]
  toJSON Description{..} = object ["Description" .= rdtDescription]
  toJSON Version{..}     = object ["Version" .= rdtVersion]
  toJSON Module{..}      = object ["Module" .= toJSON rdtModule]
  toJSON Package{..}     = object ["Package" .= toJSON rdtPackage]
  toJSON None            = toJSON Null

-- | Return the name of the type, without its details
diffTypeName :: RecipeDiffType -> String
diffTypeName Name{..}        = "Name"
diffTypeName Description{..} = "Description"
diffTypeName Version{..}     = "Version"
diffTypeName Module{..}      = "Module"
diffTypeName Package{..}     = "Package"
diffTypeName None            = "None"

-- | Return the details of the type, without its name
diffTypeDetails :: RecipeDiffType -> String
diffTypeDetails Name{..}        = rdtName
diffTypeDetails Description{..} = rdtDescription
diffTypeDetails Version{..}     = fromMaybe "" rdtVersion
diffTypeDetails Module{..}      = rmName rdtModule ++ " " ++ rmVersion rdtModule
diffTypeDetails Package{..}     = rmName rdtPackage ++ " " ++ rmVersion rdtPackage
diffTypeDetails None            = "None"

-- | A difference entry
--
-- This uses RecipeDiffType to indicate the type of difference between
-- recipe fields.
--
-- If old is set and new is Nothing it means the entry was removed
-- If old is Nothing and new is set it means the entry was added
-- If both are set then old the the old content and new is the new content
data RecipeDiffEntry = RecipeDiffEntry
    { rdeOld :: Maybe RecipeDiffType
    , rdeNew :: Maybe RecipeDiffType
    } deriving (Eq, Show)

instance FromJSON RecipeDiffEntry where
  parseJSON = withObject "Recipe diff entry" $ \o -> do
    rdeOld <- o .: "old"
    rdeNew <- o .: "new"
    return RecipeDiffEntry{..}

instance ToJSON RecipeDiffEntry where
  toJSON RecipeDiffEntry{..} = object
    [ "old" .= rdeOld
    , "new" .= rdeNew
    ]

-- | Get the type of the entry
-- They cannot be different types, but one or the other may be Nothing
entryType :: RecipeDiffEntry -> RecipeDiffType
entryType RecipeDiffEntry {rdeOld = Just t, rdeNew = Nothing}  = t
entryType RecipeDiffEntry {rdeOld = Just t, rdeNew = Just _}   = t
entryType RecipeDiffEntry {rdeOld = Nothing, rdeNew = Just t}  = t
entryType RecipeDiffEntry {rdeOld = Nothing, rdeNew = Nothing} = None

-- | The type of action represented by the RecipeDiffEntry
data RecipeDiffAction =
      Changed
    | Removed
    | Added
    deriving (Show, Eq, Ord)

-- | Return the type of action based on whether new, old, or both are set.
diffAction :: RecipeDiffEntry -> RecipeDiffAction
diffAction RecipeDiffEntry { rdeNew = Nothing } = Removed
diffAction RecipeDiffEntry { rdeOld = Nothing } = Added
diffAction _                                    = Changed

-- | Return a string representing the type of change
prettyDiffAction :: RecipeDiffAction -> String
prettyDiffAction Changed = "Changed"
prettyDiffAction Added   = "Added"
prettyDiffAction Removed = "Removed"

-- | Return a pretty string representation of the change
prettyDiffEntry :: RecipeDiffEntry -> String
prettyDiffEntry entry = change ++ " " ++ diffName ++ " " ++ diffDetail
  where
    change = prettyDiffAction $ diffAction entry
    diffName = diffTypeName $ entryType entry
    diffDetail = case diffAction entry of
        Changed -> diffChanged
        Added   -> diffTypeDetails . fromJust $ rdeNew  entry
        Removed -> diffTypeDetails . fromJust $ rdeOld  entry
    diffChanged = case entryType entry of
        Module{..}  -> oldDetails ++ " -> " ++ newModuleVersion
        Package{..} -> oldDetails ++ " -> " ++ newPackageVersion
        Version{..} -> oldDetails ++ " -> " ++ newDetails
        _           -> quoteChange oldDetails newDetails
    newModuleVersion  = rmVersion $ rdtModule  $ fromJust $ rdeNew entry
    newPackageVersion = rmVersion $ rdtPackage $ fromJust $ rdeNew entry
    oldDetails        = diffTypeDetails $ fromJust $ rdeOld entry
    newDetails        = diffTypeDetails $ fromJust $ rdeNew entry
    quoteChange o n   = "\"" ++ o ++ "\" -> \"" ++ n ++ "\""


-- | Compare the entries
-- Changes are first, followed by removals and additions
compareRecipeDiffEntry :: RecipeDiffEntry -> RecipeDiffEntry -> Ordering
compareRecipeDiffEntry a b = diffAction a `compare` diffAction b

-- | Sort a list of RecipeDiffEntry based on adding, removing, or changing entries
sortedDiff :: [RecipeDiffEntry] -> [RecipeDiffEntry]
sortedDiff = sortBy compareRecipeDiffEntry

-- | Return a pretty representation of the diff entries
prettyRecipeDiff :: [RecipeDiffEntry] -> [String]
prettyRecipeDiff entries = map prettyDiffEntry $ sortedDiff entries

-- | JSON response for /blueprints/diff
data RecipesDiffResponse = RecipesDiffResponse
    { rdrDiff :: [RecipeDiffEntry]
    } deriving (Eq, Show)

instance FromJSON RecipesDiffResponse where
  parseJSON = withObject "/blueprints/diff response" $ \o -> do
    rdrDiff <- o .: "diff"
    return RecipesDiffResponse{..}

instance ToJSON RecipesDiffResponse where
  toJSON RecipesDiffResponse{..} = object
    [ "diff" .= rdrDiff
    ]

-- | Convert the server response into the RecipesDiffResponse record
decodeRecipesDiffResponse :: Response C8.ByteString -> Maybe RecipesDiffResponse
decodeRecipesDiffResponse resp = decode $ resp ^. responseBody
