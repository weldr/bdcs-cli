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

import Control.Conditional (unlessM)
import Control.Lens ((^..), (^.), (&), (.~))
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens (_Array, _String, members, key, values, nth)
import Data.Aeson.Types (Parser, parse, parseMaybe, parseEither, object)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.HashMap.Strict as HM
import Data.List (intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.Wreq
import Network.Wreq.Session as S
import System.Directory(doesFileExist)
import System.Exit(exitFailure)
import System.IO
import Text.Printf(printf)

import Cmdline(CliOptions(..), parseArgs, helpCommand)
import API.V0

-- | Join a list of strings with a delimiter.
join delim xs = concat (intersperse delim xs)

-- | Take a list of possiby comma, or comma-space, separated options and turn it into a list of options
argify xs = filter (/= "") $ concatMap (splitOn ",") xs

-- | Return the TOML filename, ending with .toml
tomlFileName :: String -> FilePath
tomlFileName = printf "%s.toml"

-- | Return the TOML filename, ending with .frozen.toml
frozenTomlFileName :: String -> FilePath
frozenTomlFileName = tomlFileName . printf "%s.frozen"

-- | Convert the Value into a pretty JSON string for printing.
prettyJson :: Value -> String
prettyJson json = C8.unpack $ encodePretty json

-- | Print the API URL selection (or the default)
printUrl :: CliOptions -> IO ()
printUrl CliOptions{..} = putStrLn optUrl


-- | Extract the list of recipes from the server Response Value
-- a, b, c, ...
humanRecipesList :: Response Value -> String
humanRecipesList json = join ", " $ map T.unpack recipes
  where recipes = json ^.. responseBody . key "recipes" . values . _String


--
-- JSON Data types for parsing the BDCS API recipes/depsolve/ response
--

data DependencyJSON =
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


data FreezeJSON =
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
           , rVersion           :: String
           , rDescription       :: String
           , rPackages          :: [RecipeModule]
           , rModules           :: [RecipeModule]
    } deriving Show

instance FromJSON Recipe where
  parseJSON = withObject "recipe" $ \o -> do
      rName        <- o .: "name"
      rVersion     <- o .: "version"
      rDescription <- o .: "description"
      rPackages    <- o .: "packages"
      rModules     <- o .: "modules"
      return Recipe{..}

instance ToJSON Recipe where
  toJSON Recipe{..} = object [
        "name"        .= rName
      , "version"     .= rVersion
      , "description" .= rDescription
      , "packages"    .= rPackages
      , "rModules"    .= rModules ]


data RecipeModule =
    RecipeModule { rmName         :: String
                 , rmVersion      :: String
    } deriving Show

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


-- | Name and Version of a Recipe as a String
recipeNameVersion :: Recipe -> String
recipeNameVersion Recipe{..} = printf "Recipe: %s v%s" rName rVersion

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
    recipeDetails recipeDeps = [name recipeDeps] ++ dependenciesList recipeDeps
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

-- | Process the recipes list command
-- Prints a JSON or human reable list of available recipes
recipesCommand :: Session -> CliOptions -> [String] -> IO ()
recipesCommand sess opts ("list":xs) = do
    r <- listRecipes sess opts
    when (isJust r) $ do
        j <- asValue $ fromJust r
        if optJsonOutput opts
            then putStrLn $ prettyJson $ j ^. responseBody
            else putStrLn $ "Recipes: " ++ humanRecipesList j

-- | Process the recipes show command
-- Print the TOML recipe
recipesCommand sess opts ("show":xs) = showRecipe $ argify xs
  where
    showRecipe (x:xs) = do
        r <- infoRecipes sess opts x
        -- TODO This needs to check for JSON output selection...
        when (isJust r) $ putStrLn $ C8.unpack $ fromJust r ^. responseBody
        showRecipe xs
    showRecipe [] = putStrLn ""

-- | Process the recipes save command
-- Save a copy of the recipe to a TOML file using <recipe name>.toml
recipesCommand sess opts ("save":xs) = saveRecipe $ argify xs
  where
    saveRecipe (x:xs) = do
        r <- infoRecipes sess opts x
        -- TODO This needs to check for JSON output selection and save it as a .json file instead
        when (isJust r) $ writeFile (tomlFileName x) $ C8.unpack $ fromJust r ^. responseBody
        showRecipe xs
    showRecipe [] = putStrLn ""         -- How to do a 'pass' here?

-- | Process the recipe depsolve command
-- Print the list of package versions needed for the recipe list
recipesCommand sess opts ("depsolve":xs) = do
    r <- depsolveRecipes sess opts (join "," xs)
    when (isJust r) $ do
        j <- asValue $ fromJust r
        if optJsonOutput opts
            then putStrLn $ prettyJson $ j ^. responseBody
            else do
                let deps = decodeDepsolve $ fromJust r
                when (isJust deps) $ putStrLn $ join "\n\n" $ map (join "\n") $ recipesDepsList $ fromJust deps

-- | Process the recipes push command
-- Create a new recipe on the server, or overwrite an existing one, with a TOML recipe file
recipesCommand sess opts ("push":xs) = pushRecipe $ argify xs
  where
    pushRecipe (x:xs) = do
        let name = x
        unlessM (doesFileExist name) $ do
            putStrLn $ printf "ERROR: Missing file %s" name
            exitFailure
        toml <- readFile name
        newRecipes sess opts toml
        pushRecipe xs
    pushRecipe [] = putStrLn ""         -- How to do a 'pass' here?
recipesCommand _    _    (x:xs) = putStrLn $ printf "ERROR: Unknown recipes command - %s" x
recipesCommand _    _    _      = putStrLn "ERROR: Missing recipes command"

-- | Process the recipes freeze show command
-- Show the frozen recipe in TOML format
recipesFreeze sess opts ("show":xs) = showFrozenRecipe $ argify xs
  where
    showFrozenRecipe (x:xs) = do
        r <- freezeRecipeToml sess opts x
        when (isJust r) $ putStrLn $ C8.unpack $ fromJust r ^. responseBody
        showFrozenRecipe xs
    showFrozenRecipe [] = putStrLn ""

-- | Process the recipes freeze show command
-- Save the frozen recipe in TOML format, as <recipe name>.frozen.toml
recipesFreeze sess opts ("save":xs) = saveFrozenRecipe $ argify xs
  where
    saveFrozenRecipe (x:xs) = do
        r <- freezeRecipeToml sess opts x
        when (isJust r) $ writeFile (frozenTomlFileName x) $ C8.unpack $ fromJust r ^. responseBody
        saveFrozenRecipe xs
    saveFrozenRecipe [] = putStrLn ""         -- How to do a 'pass' here?

-- | Process the recipes freeze
-- Display the recipes' frozen module and packages list in human readable format
recipesFreeze sess opts xs = do
    r <- freezeRecipes sess opts (join "," xs)
    when (isJust r) $ do
        let recipes = decodeFreeze $ fromJust r
        when (isJust recipes) $ putStrLn $ join "\n\n" $ map (join "\n") $ recipesFrozenList $ fromJust recipes


-- | Process the modules list command
-- Print a list of the available modules
modulesCommand :: Session -> CliOptions -> [String] -> IO ()
modulesCommand sess opts ("list":xs)    = do
    r <- listModules sess opts
    when (isJust r) $ do
        j <- asValue $ fromJust r
        putStrLn $ prettyJson $ j ^. responseBody
modulesCommand _    _    (x:xs) = putStrLn $ printf "ERROR: Unknown modules command - %s" x
modulesCommand _    _    _      = putStrLn "ERROR: Missing modules command"

-- | Process the projects list command
-- Print a list of the available projects
projectsCommand :: Session -> CliOptions -> [String] -> IO ()
projectsCommand sess opts ("list":xs)    = do
    r <- listProjects sess opts
    when (isJust r) $ do
        j <- asValue $ fromJust r
        putStrLn $ prettyJson $ j ^. responseBody
projectsCommand sess opts ("info":xs) = do
    r <- infoProjects sess opts (join "," xs)
    when (isJust r) $ do
        j <- asValue $ fromJust r
        putStrLn $ prettyJson $ j ^. responseBody
projectsCommand _    _    (x:xs) = putStrLn $ printf "ERROR: Unknown projects command - %s" x
projectsCommand _    _    _      = putStrLn "ERROR: Missing projects command"

-- Execute a command and print the results
parseCommand :: Session -> CliOptions -> [String] -> IO ()
parseCommand sess opts ("recipes":"freeze":xs) = recipesFreeze sess opts xs
parseCommand sess opts ("recipes":xs)          = recipesCommand sess opts xs
parseCommand sess opts ("modules":xs)          = modulesCommand sess opts xs
parseCommand sess opts ("projects":xs)         = projectsCommand sess opts xs
parseCommand sess opts ("help":xs)             = helpCommand xs
parseCommand _    _    _                       = putStrLn "Unknown Command"

main :: IO ()
main = S.withSession $ \sess -> do
    r <- parseArgs
    let opts = fst r
    let commands = snd r
    when (optVerbose opts) $ do
        print opts
        print commands
        printUrl opts
    parseCommand sess opts commands
