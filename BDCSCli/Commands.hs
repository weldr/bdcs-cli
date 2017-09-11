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
import Control.Lens ((^..), (^.))
import Control.Monad(when, forM_)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens (_String, key, values)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import Network.Wreq
import Network.Wreq.Session as S
import Text.Printf(printf)
import System.Process(rawSystem)
import System.Directory(doesFileExist)
import System.Exit(exitFailure)
import System.IO(hClose,hPutStr)
import System.IO.Temp(withSystemTempFile)

import BDCSCli.API.V0
import BDCSCli.Cmdline(CliOptions(..), helpCommand)
import BDCSCli.CommandCtx(CommandCtx(..))
import BDCSCli.Utilities(argify)

-- | Return the TOML filename, ending with .toml
tomlFileName :: String -> FilePath
tomlFileName = printf "%s.toml"

-- | Return the TOML filename, ending with .frozen.toml
frozenTomlFileName :: String -> FilePath
frozenTomlFileName = tomlFileName . printf "%s.frozen"

-- | Return the tar filename, ending with .tar
tarFileName :: String -> FilePath
tarFileName = printf "%s.tar"

-- | Convert the Value into a pretty JSON string for printing.
prettyJson :: Value -> String
prettyJson jsonValue = C8.unpack $ encodePretty jsonValue

-- | Extract the list of recipes from the server Response Value
-- a, b, c, ...
humanRecipesList :: Response Value -> String
humanRecipesList jsonValue = intercalate ", " $ map T.unpack recipes
  where recipes = jsonValue ^.. responseBody . key "recipes" . values . _String

-- | Process the compose types command
-- Prints a list of the supported compose types
composeCommand :: CommandCtx -> [String] -> IO ()
composeCommand _ ("types":_) =
    -- API has a list of compose types, but we are not currently using that
    putStrLn "tar"

composeCommand _   ["tar"]          = putStrLn "ERROR: Missing recipe name"
composeCommand ctx ("tar":recipe:_) = depsolveRecipes ctx recipe >>= \r -> do
    let deps = decodeDepsolve $ fromJust r
--    when (isJust deps) $ composeTar opts $ fromJust deps
    forM_ deps composeTar
  where
    composeTar deps = withSystemTempFile "bdcs-deps-" $ \tmpFile hFile -> do
        -- write deps to tmpFile
        hPutStr hFile $ intercalate "\n" $ getDepNEVRAList deps
        hClose hFile
        let tarFile = tarFileName recipe
        let mddbPath = optMDDB $ ctxOptions ctx
        let repoPath = optRepo $ ctxOptions ctx
        printf "Creating tar of %s, saving to %s\n" recipe tarFile
        rawSystem "export" [mddbPath, repoPath, tarFile, tmpFile]

composeCommand _    _      = putStrLn "ERROR: Unknown compose type"


-- | Process the recipes list command
-- Prints a JSON or human reable list of available recipes
recipesCommand :: CommandCtx -> [String] -> IO ()
recipesCommand ctx ("list":_) = listRecipes ctx >>= \r -> do
    j <- asValue $ fromJust r
    if optJsonOutput $ ctxOptions ctx
        then putStrLn $ prettyJson $ j ^. responseBody
        else putStrLn $ "Recipes: " ++ humanRecipesList j

-- | Process the recipes show command
-- Print the TOML recipe
recipesCommand ctx ("show":xs) = showRecipe $ argify xs
  where
    showRecipe (x:xxs) = infoRecipes ctx x >>= \r -> do
        -- TODO This needs to check for JSON output selection...
        putStrLn $ C8.unpack $ fromJust r ^. responseBody
        showRecipe xxs
    showRecipe [] = putStrLn ""

-- | Process the recipes save command
-- Save a copy of the recipe to a TOML file using <recipe name>.toml
recipesCommand ctx ("save":xs) = saveRecipe $ argify xs
  where
    saveRecipe (x:xxs) = infoRecipes ctx x >>= \r -> do
        -- TODO This needs to check for JSON output selection and save it as a .json file instead
        writeFile (tomlFileName x) $ C8.unpack $ fromJust r ^. responseBody
        saveRecipe xxs
    saveRecipe [] = putStrLn ""         -- How to do a 'pass' here?

-- | Process the recipe depsolve command
-- Print the list of package versions needed for the recipe list
recipesCommand ctx ("depsolve":xs) = depsolveRecipes ctx (intercalate "," xs) >>= \r -> do
    j <- asValue $ fromJust r
    if optJsonOutput $ ctxOptions ctx
        then putStrLn $ prettyJson $ j ^. responseBody
        else do
            let deps = decodeDepsolve $ fromJust r
            when (isJust deps) $ putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ recipesDepsList $ fromJust deps

-- | Process the recipes push command
-- Create a new recipe on the server, or overwrite an existing one, with a TOML recipe file
recipesCommand ctx ("push":xs) = pushRecipe $ argify xs
  where
    pushRecipe (x:xxs) = do
        let name = x
        unlessM (doesFileExist name) $ do
            putStrLn $ printf "ERROR: Missing file %s" name
            exitFailure
        toml <- readFile name
        newRecipes ctx toml
        pushRecipe xxs
    pushRecipe [] = putStrLn ""         -- How to do a 'pass' here?
recipesCommand _    (x:_) = putStrLn $ printf "ERROR: Unknown recipes command - %s" x
recipesCommand _    _     = putStrLn "ERROR: Missing recipes command"

-- | Process the recipes freeze show command
-- Show the frozen recipe in TOML format
recipesFreeze :: CommandCtx -> [String] -> IO ()
recipesFreeze ctx ("show":xs) = showFrozenRecipe $ argify xs
  where
    showFrozenRecipe (x:xxs) = freezeRecipeToml ctx x >>= \r -> do
        putStrLn $ C8.unpack $ fromJust r ^. responseBody
        showFrozenRecipe xxs
    showFrozenRecipe [] = putStrLn ""

-- | Process the recipes freeze show command
-- Save the frozen recipe in TOML format, as <recipe name>.frozen.toml
recipesFreeze ctx ("save":xs) = saveFrozenRecipe $ argify xs
  where
    saveFrozenRecipe (x:xxs) = freezeRecipeToml ctx x >>= \r -> do
        writeFile (frozenTomlFileName x) $ C8.unpack $ fromJust r ^. responseBody
        saveFrozenRecipe xxs
    saveFrozenRecipe [] = putStrLn ""         -- How to do a 'pass' here?

-- | Process the recipes freeze
-- Display the recipes' frozen module and packages list in human readable format
recipesFreeze ctx xs = freezeRecipes ctx (intercalate "," xs) >>= \r -> do
    let recipes = decodeFreeze $ fromJust r
    when (isJust recipes) $ putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ recipesFrozenList $ fromJust recipes


-- | Process the modules list command
-- Print a list of the available modules
modulesCommand :: CommandCtx -> [String] -> IO ()
modulesCommand ctx ("list":_) = listModules ctx >>= \r -> do
    j <- asValue $ fromJust r
    putStrLn $ prettyJson $ j ^. responseBody
modulesCommand _    (x:_) = putStrLn $ printf "ERROR: Unknown modules command - %s" x
modulesCommand _    _     = putStrLn "ERROR: Missing modules command"

-- | Process the projects list command
-- Print a list of the available projects
projectsCommand :: CommandCtx -> [String] -> IO ()
projectsCommand ctx ("list":_)    = listProjects ctx >>= \r -> do
    j <- asValue $ fromJust r
    putStrLn $ prettyJson $ j ^. responseBody
projectsCommand ctx ("info":xs) = infoProjects ctx (intercalate "," xs) >>= \r -> do
    j <- asValue $ fromJust r
    putStrLn $ prettyJson $ j ^. responseBody
projectsCommand _    (x:_) = putStrLn $ printf "ERROR: Unknown projects command - %s" x
projectsCommand _    _     = putStrLn "ERROR: Missing projects command"

-- Execute a command and print the results
parseCommand :: CommandCtx -> [String] -> IO ()
parseCommand ctx ("compose":xs)          = composeCommand ctx xs
parseCommand ctx ("recipes":"freeze":xs) = recipesFreeze ctx xs
parseCommand ctx ("recipes":xs)          = recipesCommand ctx xs
parseCommand ctx ("modules":xs)          = modulesCommand ctx xs
parseCommand ctx ("projects":xs)         = projectsCommand ctx xs
parseCommand _   ("help":xs)             = helpCommand xs
parseCommand _   []                      = helpCommand []
parseCommand _   _                       = putStrLn "Unknown Command"
