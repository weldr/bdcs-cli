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
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(ReaderT, ask)
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
composeCommand :: [String] -> ReaderT CommandCtx IO ()
composeCommand ("types":_) =
    -- API has a list of compose types, but we are not currently using that
    liftIO $ putStrLn "tar"

composeCommand ("tar":[])       = liftIO $ putStrLn "ERROR: Missing recipe name"
composeCommand ("tar":recipe:_) = do
    r <- depsolveRecipes recipe
    when (isJust r) $ do
        let deps = decodeDepsolve $ fromJust r
--        when (isJust deps) $ composeTar opts $ fromJust deps
        forM_ deps composeTar
  where
    composeTar :: DependencyJSON -> ReaderT CommandCtx IO ()
    composeTar deps = do
        opts <- ctxOptions <$> ask
        liftIO $ withSystemTempFile "bdcs-deps-" $ \tmpFile hFile -> do
            -- write deps to tmpFile
            hPutStr hFile $ intercalate "\n" $ getDepNEVRAList deps
            hClose hFile
            let tarFile = tarFileName recipe
            let mddbPath = optMDDB opts
            let repoPath = optRepo opts
            printf "Creating tar of %s, saving to %s\n" recipe tarFile
            rawSystem "export" [mddbPath, repoPath, tarFile, tmpFile]
            return ()

composeCommand _      = liftIO $ putStrLn "ERROR: Unknown compose type"


-- | Process the recipes list command
-- Prints a JSON or human reable list of available recipes
recipesCommand :: [String] -> ReaderT CommandCtx IO ()
recipesCommand ("list":_) = do
    r <- listRecipes
    when (isJust r) $ do
        j <- asValue $ fromJust r
        opts <- ctxOptions <$> ask
        if optJsonOutput opts
            then liftIO $ putStrLn $ prettyJson $ j ^. responseBody
            else liftIO $ putStrLn $ "Recipes: " ++ humanRecipesList j

-- | Process the recipes show command
-- Print the TOML recipe
recipesCommand ("show":xs) = showRecipe $ argify xs
  where
    showRecipe (x:xxs) = do
        r <- infoRecipes x
        -- TODO This needs to check for JSON output selection...
        when (isJust r) $ liftIO $ putStrLn $ C8.unpack $ fromJust r ^. responseBody
        showRecipe xxs
    showRecipe [] = liftIO $ putStrLn ""

-- | Process the recipes save command
-- Save a copy of the recipe to a TOML file using <recipe name>.toml
recipesCommand ("save":xs) = saveRecipe $ argify xs
  where
    saveRecipe (x:xxs) = do
        r <- infoRecipes x
        -- TODO This needs to check for JSON output selection and save it as a .json file instead
        when (isJust r) $ liftIO $ writeFile (tomlFileName x) $ C8.unpack $ fromJust r ^. responseBody
        saveRecipe xxs
    saveRecipe [] = liftIO $ putStrLn ""         -- How to do a 'pass' here?

-- | Process the recipe depsolve command
-- Print the list of package versions needed for the recipe list
recipesCommand ("depsolve":xs) = do
    r <- depsolveRecipes (intercalate "," xs)
    when (isJust r) $ do
        j <- asValue $ fromJust r
        opts <- ctxOptions <$> ask
        if optJsonOutput opts
            then liftIO $ putStrLn $ prettyJson $ j ^. responseBody
            else do
                let deps = decodeDepsolve $ fromJust r
                when (isJust deps) $ liftIO $ putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ recipesDepsList $ fromJust deps

-- | Process the recipes push command
-- Create a new recipe on the server, or overwrite an existing one, with a TOML recipe file
recipesCommand ("push":xs) = pushRecipe $ argify xs
  where
    pushRecipe :: [String] -> ReaderT CommandCtx IO ()
    pushRecipe (x:xxs) = do
        let name = x
        unlessM (liftIO $ doesFileExist name) $ do
            liftIO $ putStrLn $ printf "ERROR: Missing file %s" name
            liftIO $ exitFailure
        toml <- liftIO $ readFile name
        newRecipes toml
        pushRecipe xxs
    pushRecipe [] = liftIO $ putStrLn ""         -- How to do a 'pass' here?
recipesCommand (x:_) = liftIO $ putStrLn $ printf "ERROR: Unknown recipes command - %s" x
recipesCommand _     = liftIO $ putStrLn "ERROR: Missing recipes command"

-- | Process the recipes freeze show command
-- Show the frozen recipe in TOML format
recipesFreeze :: [String] -> ReaderT CommandCtx IO ()
recipesFreeze ("show":xs) = showFrozenRecipe $ argify xs
  where
    showFrozenRecipe (x:xxs) = do
        r <- freezeRecipeToml x
        when (isJust r) $ liftIO $ putStrLn $ C8.unpack $ fromJust r ^. responseBody
        showFrozenRecipe xxs
    showFrozenRecipe [] = liftIO $ putStrLn ""

-- | Process the recipes freeze show command
-- Save the frozen recipe in TOML format, as <recipe name>.frozen.toml
recipesFreeze ("save":xs) = saveFrozenRecipe $ argify xs
  where
    saveFrozenRecipe (x:xxs) = do
        r <- freezeRecipeToml x
        when (isJust r) $ liftIO $ writeFile (frozenTomlFileName x) $ C8.unpack $ fromJust r ^. responseBody
        saveFrozenRecipe xxs
    saveFrozenRecipe [] = liftIO $ putStrLn ""         -- How to do a 'pass' here?

-- | Process the recipes freeze
-- Display the recipes' frozen module and packages list in human readable format
recipesFreeze xs = do
    r <- freezeRecipes (intercalate "," xs)
    when (isJust r) $ do
        let recipes = decodeFreeze $ fromJust r
        when (isJust recipes) $ liftIO $ putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ recipesFrozenList $ fromJust recipes


-- | Process the modules list command
-- Print a list of the available modules
modulesCommand :: [String] -> ReaderT CommandCtx IO ()
modulesCommand ("list":_)    = do
    r <- listModules
    when (isJust r) $ do
        j <- asValue $ fromJust r
        liftIO $ putStrLn $ prettyJson $ j ^. responseBody
modulesCommand (x:_) = liftIO $ putStrLn $ printf "ERROR: Unknown modules command - %s" x
modulesCommand _     = liftIO $ putStrLn "ERROR: Missing modules command"

-- | Process the projects list command
-- Print a list of the available projects
projectsCommand :: [String] -> ReaderT CommandCtx IO ()
projectsCommand ("list":_)    = do
    r <- listProjects
    when (isJust r) $ do
        j <- asValue $ fromJust r
        liftIO $ putStrLn $ prettyJson $ j ^. responseBody
projectsCommand ("info":xs) = do
    r <- infoProjects (intercalate "," xs)
    when (isJust r) $ do
        j <- asValue $ fromJust r
        liftIO $ putStrLn $ prettyJson $ j ^. responseBody
projectsCommand (x:_) = liftIO $ putStrLn $ printf "ERROR: Unknown projects command - %s" x
projectsCommand _     = liftIO $ putStrLn "ERROR: Missing projects command"

-- Execute a command and print the results
parseCommand :: [String] -> ReaderT CommandCtx IO ()
parseCommand ("compose":xs)          = composeCommand xs
parseCommand ("recipes":"freeze":xs) = recipesFreeze xs
parseCommand ("recipes":xs)          = recipesCommand xs
parseCommand ("modules":xs)          = modulesCommand xs
parseCommand ("projects":xs)         = projectsCommand xs
parseCommand ("help":xs)             = liftIO $ helpCommand xs
parseCommand _                       = liftIO $ putStrLn "Unknown Command"
