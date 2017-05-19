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

module BDCSCli.Commands(parseCommand)
  where

import Control.Conditional (unlessM)
import Control.Lens ((^..), (^.), (&), (.~))
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens (_Array, _String, members, key, values, nth)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import Network.Wreq
import Network.Wreq.Session as S
import Text.Printf(printf)
import System.Directory(doesFileExist)
import System.Exit(exitFailure)

import BDCSCli.API.V0
import BDCSCli.Cmdline(CliOptions(..), helpCommand)
import BDCSCli.Utilities(argify, join)

-- | Return the TOML filename, ending with .toml
tomlFileName :: String -> FilePath
tomlFileName = printf "%s.toml"

-- | Return the TOML filename, ending with .frozen.toml
frozenTomlFileName :: String -> FilePath
frozenTomlFileName = tomlFileName . printf "%s.frozen"

-- | Convert the Value into a pretty JSON string for printing.
prettyJson :: Value -> String
prettyJson json = C8.unpack $ encodePretty json

-- | Extract the list of recipes from the server Response Value
-- a, b, c, ...
humanRecipesList :: Response Value -> String
humanRecipesList json = join ", " $ map T.unpack recipes
  where recipes = json ^.. responseBody . key "recipes" . values . _String

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


