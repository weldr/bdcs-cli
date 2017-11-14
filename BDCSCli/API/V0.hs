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

module BDCSCli.API.V0(listRecipes,
                      infoRecipes,
                      depsolveRecipes,
                      freezeRecipeToml,
                      freezeRecipes,
                      newRecipes,
                      listModules,
                      listProjects,
                      infoProjects,
                      decodeDepsolve,
                      decodeFreeze,
                      recipesDepsList,
                      recipesFrozenList,
                      getDepNEVRAList,
                      Recipe(..),
                      RecipeModule(..))
  where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe(fromMaybe)
import Network.Wreq
import Text.Printf(printf)

import BDCSCli.CommandCtx(CommandCtx(..))
import BDCSCli.URL(apiUrl, getUrl, postUrl)

-- | Request the list of recipes from the API server
listRecipes :: CommandCtx -> IO (Maybe (Response BSL.ByteString))
listRecipes CommandCtx{..} = getUrl ctxSession $ apiUrl ctxOptions "recipes/list"

-- | Request the TOML copy of the Recipe from the API server
infoRecipes :: CommandCtx -> String -> IO (Maybe (Response BSL.ByteString))
infoRecipes CommandCtx{..} recipe = getUrl ctxSession $ apiUrl ctxOptions "recipes/info/" ++ recipe ++ "?format=toml"

-- | Request the dependecies for the recipe from the APO server
depsolveRecipes :: CommandCtx -> String -> IO (Maybe (Response BSL.ByteString))
depsolveRecipes CommandCtx{..} recipes = getUrl ctxSession $ apiUrl ctxOptions "recipes/depsolve/" ++ recipes

-- | Request the frozen recipe from the API server in TOML format
freezeRecipeToml :: CommandCtx -> String -> IO (Maybe (Response BSL.ByteString))
freezeRecipeToml CommandCtx{..} recipe = getUrl ctxSession $ apiUrl ctxOptions "recipes/freeze/" ++ recipe ++ "?format=toml"

-- | Request the frozen recipe from the API server in JSON format
freezeRecipes :: CommandCtx -> String -> IO (Maybe (Response BSL.ByteString))
freezeRecipes CommandCtx{..} recipes = getUrl ctxSession $ apiUrl ctxOptions "recipes/freeze/" ++ recipes

{-# ANN newRecipes ("HLint: ignore Eta reduce"::String) #-}
-- | POST a new TOML recipe to the API server
newRecipes :: CommandCtx -> String -> IO (Maybe (Response BSL.ByteString))
newRecipes CommandCtx{..} bodyStr = postUrl ctxSession (apiUrl ctxOptions "recipes/new") bodyStr

-- | Request a list of the available modules from the API server
listModules :: CommandCtx -> IO (Maybe (Response BSL.ByteString))
listModules CommandCtx{..} = getUrl ctxSession $ apiUrl ctxOptions "modules/list"

-- | Request a list of the available projects from the API server
listProjects :: CommandCtx -> IO (Maybe (Response BSL.ByteString))
listProjects CommandCtx{..} = getUrl ctxSession $ apiUrl ctxOptions "projects/list"

-- | Request detailed info for a list of projects
infoProjects :: CommandCtx -> String -> IO (Maybe (Response BSL.ByteString))
infoProjects CommandCtx{..} projects = getUrl ctxSession $ apiUrl ctxOptions "projects/info/" ++ projects


--
-- JSON Data types for parsing the BDCS API recipes/depsolve/ response
--

newtype DependencyJSON =
    DependencyJSON { djRecipes  :: [RecipeDeps]
    } deriving Show

instance FromJSON DependencyJSON where
  parseJSON = withObject "dependency JSON" $ \o -> do
      djRecipes <- o .: "recipes"
      return DependencyJSON{..}

instance ToJSON DependencyJSON where
  toJSON DependencyJSON{..} = object [
        "recipes" .= djRecipes ]


data RecipeDeps =
    RecipeDeps { rdRecipe       :: Recipe
               , rdModules      :: [PackageNEVRA]
               , rdDependencies :: [PackageNEVRA]
    } deriving Show

instance FromJSON RecipeDeps where
  parseJSON = withObject "recipe deps" $ \o -> do
      rdRecipe       <- o .: "recipe"
      rdModules      <- o .: "modules"
      rdDependencies <- o .: "dependencies"
      return RecipeDeps{..}

instance ToJSON RecipeDeps where
  toJSON RecipeDeps{..} = object [
        "recipe"       .= rdRecipe
      , "modules"      .= rdModules
      , "dependencies" .= rdDependencies ]


newtype FreezeJSON =
    FreezeJSON { fjRecipes :: [Recipe]
    } deriving Show

instance FromJSON FreezeJSON where
  parseJSON = withObject "freeze JSON" $ \o -> do
    fjRecipes <- o .: "recipes"
    return FreezeJSON{..}

instance ToJSON FreezeJSON where
  toJSON FreezeJSON{..} = object [
    "recipes" .= fjRecipes ]


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


-- | Package build details
data PackageNEVRA =
    PackageNEVRA { pnName       :: String
                 , pnEpoch      :: Int
                 , pnVersion    :: String
                 , pnRelease    :: String
                 , pnArch       :: String
    } deriving Show

instance FromJSON PackageNEVRA where
  parseJSON = withObject "package NEVRA" $ \o -> do
      pnName    <- o .: "name"
      pnEpoch   <- o .: "epoch"
      pnVersion <- o .: "version"
      pnRelease <- o .: "release"
      pnArch    <- o .: "arch"
      return PackageNEVRA{..}

instance ToJSON PackageNEVRA where
  toJSON PackageNEVRA{..} = object [
        "name" .= pnName
      , "epoch" .= pnEpoch
      , "version" .= pnVersion
      , "release" .= pnRelease
      , "arch" .= pnArch ]

--
-- Functions for manipulating/extracting the API data
--

-- | Name and Version of a Recipe as a String
recipeNameVersion :: Recipe -> String
recipeNameVersion Recipe{..} = printf "Recipe: %s v%s" rName (fromMaybe "" rVersion)

-- | Description of a Recipe as a String
recipeDescription :: Recipe -> String
recipeDescription Recipe{..} = printf "    %s" rDescription

-- | Package NEVRA as a String, only including epoch when it is > 0
packageNEVRA :: PackageNEVRA -> String
packageNEVRA PackageNEVRA{..} =
    if pnEpoch == 0
    then printf "%s-%s-%s.%s" pnName pnVersion pnRelease pnArch
    else printf "%s-%d:%s-%s.%s" pnName pnEpoch pnVersion pnRelease pnArch

moduleNameVersion :: RecipeModule -> String
moduleNameVersion RecipeModule{..} = printf "%s-%s" rmName rmVersion

-- | Get the recipe's modules
getModules :: Recipe -> [RecipeModule]
getModules Recipe{..} = rModules

-- | Get the recipe's packages
getPackages :: Recipe -> [RecipeModule]
getPackages Recipe{..} = rPackages

-- | Get the Recipe from the depsolve results
getDepRecipe :: RecipeDeps -> Recipe
getDepRecipe RecipeDeps{..} = rdRecipe

-- | Get the module list from the depsolve
-- includes the exact versions of the recipe's modules and packages
getDepModules :: RecipeDeps -> [PackageNEVRA]
getDepModules RecipeDeps{..} = rdModules

-- | Get all the dependencies needed for the recipe
getDependencies :: RecipeDeps -> [PackageNEVRA]
getDependencies RecipeDeps{..} = rdDependencies

-- | Get the depsolved recipes from the request results
getDepRecipes :: DependencyJSON -> [RecipeDeps]
getDepRecipes DependencyJSON{..} = djRecipes

-- | Get the frozen recipes from the request results
getFrozenRecipes :: FreezeJSON -> [Recipe]
getFrozenRecipes FreezeJSON{..} = fjRecipes

-- | Get the recipes' dependencies as a list of PackageNEVRA Strings
getDepNEVRAList :: DependencyJSON -> [String]
getDepNEVRAList deps = map packageNEVRA $ concatMap getDependencies $ getDepRecipes deps

-- | Convert the server response into data structures
decodeDepsolve :: Response C8.ByteString -> Maybe DependencyJSON
decodeDepsolve resp = decode $ resp ^. responseBody

-- | Extract the names and versions of the recipes in the depsolve response
recipesList :: DependencyJSON -> [String]
recipesList deps = map (recipeNameVersion . getDepRecipe) $ getDepRecipes deps

-- | Extract the dependency list from the depsolve response
dependenciesList :: RecipeDeps -> [String]
dependenciesList recipeDeps = map (\p -> "    " ++ packageNEVRA p) $ getDependencies recipeDeps

-- | Return the dependencies for each recipe in the depsolve response
-- Returns a list of strings for each recipe, a list of lists where the outer list is one per recipe.
recipesDepsList :: DependencyJSON -> [[String]]
recipesDepsList deps = map recipeDetails $ getDepRecipes deps
  where
    recipeDetails recipeDeps = name recipeDeps : dependenciesList recipeDeps
    name = recipeNameVersion . getDepRecipe

-- | Convert the server response into data structures
decodeFreeze :: Response C8.ByteString -> Maybe FreezeJSON
decodeFreeze resp = decode $ resp ^. responseBody

{-# ANN recipesFrozenList ("HLint: ignore Eta reduce"::String) #-}
-- | Return the module and package versions for each recipe.
-- Returns a list of strings for each recipe, a list of lists where the outer list is one per recipe.
recipesFrozenList :: FreezeJSON -> [[String]]
recipesFrozenList recipes = map recipeDetails $ getFrozenRecipes recipes
  where
    recipeDetails recipe = [recipeNameVersion recipe] ++ moduleDetails recipe ++ packageDetails recipe
    moduleDetails recipe = map (\m -> "    " ++ moduleNameVersion m) $ getModules recipe
    packageDetails recipe = map (\p -> "    " ++ moduleNameVersion p) $ getPackages recipe
