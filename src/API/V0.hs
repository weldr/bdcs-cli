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
module API.V0(listRecipes,
              infoRecipes,
              depsolveRecipes,
              freezeRecipeToml,
              freezeRecipes,
              newRecipes,
              listModules,
              listProjects,
              infoProjects)
  where

import qualified Data.ByteString.Lazy as BSL
import Network.Wreq(Response)
import Network.Wreq.Session(Session)

import Cmdline(CliOptions(..))
import URL(apiUrl,
           getUrl,
           postUrl)

-- | Request the list of recipes from the API server
listRecipes :: Session -> CliOptions -> IO (Maybe (Response BSL.ByteString))
listRecipes sess opts = getUrl sess $ apiUrl opts "recipes/list"

-- | Request the TOML copy of the Recipe from the API server
infoRecipes :: Session -> CliOptions -> String -> IO (Maybe (Response BSL.ByteString))
infoRecipes sess opts recipe = getUrl sess $ apiUrl opts "recipes/info/" ++ recipe ++ "?format=toml"

-- | Request the dependecies for the recipe from the APO server
depsolveRecipes :: Session -> CliOptions -> String -> IO (Maybe (Response BSL.ByteString))
depsolveRecipes sess opts recipes = getUrl sess $ apiUrl opts "recipes/depsolve/" ++ recipes

-- | Request the frozen recipe from the API server in TOML format
freezeRecipeToml :: Session -> CliOptions -> String -> IO (Maybe (Response BSL.ByteString))
freezeRecipeToml sess opts recipe = getUrl sess $ apiUrl opts "recipes/freeze/" ++ recipe ++ "?format=toml"

-- | Request the frozen recipe from the API server in JSON format
freezeRecipes :: Session -> CliOptions -> String -> IO (Maybe (Response BSL.ByteString))
freezeRecipes sess opts recipes = getUrl sess $ apiUrl opts "recipes/freeze/" ++ recipes

{-# ANN newRecipes ("HLint: ignore Eta reduce"::String) #-}
-- | POST a new TOML recipe to the API server
newRecipes :: Session -> CliOptions -> String -> IO (Maybe (Response BSL.ByteString))
newRecipes sess opts bodyStr = postUrl sess (apiUrl opts "recipes/new") bodyStr

-- | Request a list of the available modules from the API server
listModules :: Session -> CliOptions -> IO (Maybe (Response BSL.ByteString))
listModules sess opts = getUrl sess $ apiUrl opts "modules/list"

-- | Request a list of the available projects from the API server
listProjects :: Session -> CliOptions -> IO (Maybe (Response BSL.ByteString))
listProjects sess opts = getUrl sess $ apiUrl opts "projects/list"

-- | Request detailed info for a list of projects
infoProjects :: Session -> CliOptions -> String -> IO (Maybe (Response BSL.ByteString))
infoProjects sess opts projects = getUrl sess $ apiUrl opts "projects/info/" ++ projects



