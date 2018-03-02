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

module BDCSCli.API.Types.Recipe(
    Recipe(..),
    RecipeModule(..)
) where

import           Data.Aeson
import           Data.Maybe(fromMaybe)

data RecipeModule =
    RecipeModule { rmName         :: String
                 , rmVersion      :: String
    } deriving (Eq, Show)

instance FromJSON RecipeModule where
  parseJSON = withObject "recipe module" $ \o -> do
      rmName    <- o .: "name"
      rmVersion <- o .: "version"
      return RecipeModule{..}

instance ToJSON RecipeModule where
  toJSON RecipeModule{..} = object [
        "name"    .= rmName
      , "version" .= rmVersion ]

data Recipe =
    Recipe { rName              :: String
           , rVersion           :: Maybe String
           , rDescription       :: String
           , rPackages          :: [RecipeModule]
           , rModules           :: [RecipeModule]
    } deriving (Eq, Show)

instance FromJSON Recipe where
  parseJSON = withObject "recipe" $ \o -> do
      rName        <- o .:  "name"
      rVersion     <- o .:? "version"
      rDescription <- o .:  "description"
      rPackages    <- o .:? "packages" .!= []
      rModules     <- o .:? "modules" .!= []
      return Recipe{..}

instance ToJSON Recipe where
  toJSON Recipe{..} = object [
        "name"        .= rName
      , "version"     .= fromMaybe "" rVersion
      , "description" .= rDescription
      , "packages"    .= rPackages
      , "rModules"    .= rModules ]

