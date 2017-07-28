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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module BDCSCli.Recipe(parseRecipe)
  where

import           Data.Aeson(toJSON, fromJSON)
import           Data.Aeson.Types(Result(..))
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Text.Printf(printf)
import           Text.Toml(parseTomlDoc)

import           BDCSCli.API.V0(Recipe(..), RecipeModule(..))

-- | Parse a TOML formatted string and return a Recipe
parseRecipe :: T.Text -> Either String Recipe
parseRecipe xs =
    case parseTomlDoc "" xs of
        Left err    -> Left ("Parsing TOML document failed. " ++ show err)
        Right table -> do
            let json = toJSON table
            case (fromJSON json :: Result Recipe) of
                Error err -> Left ("Converting from JSON to Recipe failed. " ++ show err)
                Success r -> Right r

-- | Convert a Recipe to a TOML string
recipeTOML :: Recipe -> T.Text
recipeTOML Recipe{..} = T.concat [nameText, versionText, descriptionText, modulesText, packagesText]
  where
    nameText = T.pack $ printf "name = \"%s\"\n" rName
    versionText = T.pack $ printf "version = \"%s\"\n" $ fromMaybe "" rVersion
    descriptionText = T.pack $ printf "description = \"%s\"\n\n" rDescription

    moduleText :: T.Text -> RecipeModule -> T.Text
    moduleText name RecipeModule{..} = T.pack $ printf "[[%s]]\nname = \"%s\"\nversion = \"%s\"\n\n" name rmName rmVersion
    packagesText = T.concat $ map (moduleText "packages") rPackages
    modulesText = T.concat $ map (moduleText "modules") rModules
