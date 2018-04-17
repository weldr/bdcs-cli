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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BDCSCli.Commands(parseCommand)
  where

import           Control.Conditional (unlessM)
import           Control.Lens ((^..), (^.))
import           Control.Monad(when, unless)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Lens (_String, key, values)
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List (intercalate)
import           Data.Maybe (isJust, fromJust)
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import           Network.Wreq
import           Text.Printf(printf)
import           System.Directory(doesFileExist)
import           System.Exit(exitFailure)

import           BDCSCli.API.V0
import           BDCSCli.Cmdline(CliOptions(..), helpCommand)
import           BDCSCli.CommandCtx(CommandCtx(..))
import           BDCSCli.Utilities(argify)

-- | Return the TOML filename, ending with .toml
tomlFileName :: String -> FilePath
tomlFileName = printf "%s.toml"

-- | Return the TOML filename, ending with .frozen.toml
frozenTomlFileName :: String -> FilePath
frozenTomlFileName = tomlFileName . printf "%s.frozen"

-- | Convert the Value into a pretty JSON string for printing.
prettyJson :: Value -> String
prettyJson jsonValue = C8.unpack $ encodePretty jsonValue

-- | Extract the list of recipes from the server Response Value
-- a, b, c, ...
humanRecipesList :: Response Value -> String
humanRecipesList jsonValue = intercalate ", " $ map T.unpack recipes
  where recipes = jsonValue ^.. responseBody . key "blueprints" . values . _String

-- | Print the error messages from an API response
printErrors :: CommandCtx -> [String] -> IO ()
printErrors ctx errors = unless (isJSONOutput ctx) $ mapM_ putStrLn errors

-- | Return true if JSON output has been selected
isJSONOutput :: CommandCtx -> Bool
isJSONOutput ctx = optJsonOutput $ ctxOptions ctx

-- | Pretty print the JSON from the response
printJSON :: CommandCtx -> Response Value -> IO ()
printJSON ctx v = when (isJSONOutput ctx) $ putStrLn $ prettyJson $ v ^. responseBody

-- | Handle printing the errors from an API response
-- optionally print the raw JSON
-- Prints errors if there is no server response, or if it cannot decode the JSON
handleAPIResponse :: CommandCtx -> Maybe (Response C8.ByteString) -> IO ()
handleAPIResponse ctx mresp = case mresp of
    Nothing -> putStrLn "ERROR: No server response"
    Just r -> do
        j <- asValue r

        printJSON ctx j
        printErrors ctx $ errors r

    -- TODO Return a status to use for the exit code
  where
    errors r = case decodeAPIResponse r of
        Nothing  -> ["ERROR: Cannot decode JSON response"]
        Just r' -> arjErrors r'

-- | Process the compose types command
-- Prints a list of the supported compose types
composeCommand :: CommandCtx -> [String] -> IO ()
composeCommand ctx ("types":_) = composeTypes ctx >>= \case
    Nothing -> putStrLn "ERROR: No server response"
    Just r  -> do
        j <- asValue r
        if isJSONOutput ctx
            then putStrLn $ prettyJson $ j ^. responseBody
            else case decodeComposeTypesResponse r of
                Just cTypes -> putStrLn $ "Compose types: " ++ intercalate ", " (composeTypesList cTypes)
                Nothing     -> putStrLn "ERROR: Cannot decode JSON response"

-- | Start a compose of a recipe
-- TODO Add support for --test=X cmdline option
-- TODO Add support for --branch=X cmdline option
composeCommand _   ["start"]                 = putStrLn "ERROR: missing blueprint and compose type"
composeCommand _   ["start", _]              = putStrLn "ERROR: missing compose type"
composeCommand ctx ("start":blueprint:ctype:_) =
    composeStart ctx composeBodyString >>= \case
        Nothing -> putStrLn "ERROR: No server response"
        Just r  -> case r ^. responseStatus . statusCode of
            400 -> handleAPIResponse ctx $ Just r
            200 -> printBuildUUID r
            s   -> printf "ERROR: Unknown status %d: %s\n" s (cs $ r ^. responseStatus . statusMessage :: String)
  where
    composeBodyString = C8.unpack $ encode $ toJSON $ ComposeBody bpText ctText Nothing
    bpText = T.pack blueprint
    ctText = T.pack ctype
    printBuildUUID r = do
        j <- asValue r
        if isJSONOutput ctx
            then putStrLn $ prettyJson $ j ^. responseBody
            else case decodeComposeResponse r of
                Just build  -> printf "Compose %s added to the queue\n" (crBuildID build)
                Nothing     -> putStrLn "ERROR: Cannot decode JSON response"

-- | Print the status of the queue, all finished and all failed builds
composeCommand ctx ("status":_) = do
    queue >>= \case
        (Nothing, _)         -> putStrLn "ERROR: No server response"
        (Just r, [new, run]) -> do
            j <- asValue r
            if isJSONOutput ctx
                then putStrLn $ prettyJson $ j ^. responseBody
                else mapM_ (printf "%s") $ new ++ run

    finished >>= \case
        (Nothing, _)         -> putStrLn "ERROR: No server response"
        (Just r, f) -> do
            j <- asValue r
            if isJSONOutput ctx
                then putStrLn $ prettyJson $ j ^. responseBody
                else mapM_ (printf "%s") f

    failed >>= \case
        (Nothing, _)         -> putStrLn "ERROR: No server response"
        (Just r, f) -> do
            j <- asValue r
            if isJSONOutput ctx
                then putStrLn $ prettyJson $ j ^. responseBody
                else mapM_ (printf "%s") f
  where
    queue = composeQueue ctx >>= \case
        Nothing -> return (Nothing, [])
        Just r  -> case decodeComposeQueueResponse r of
            Nothing      -> return (Just r, [])
            Just qStatus -> return (Just r, [cqrNew qStatus, cqrRun qStatus])

    finished = composeFinished ctx >>= \case
        Nothing -> return (Nothing, [])
        Just r  -> case decodeComposeFinishedResponse r of
            Nothing -> return (Just r, [])
            Just f  -> return (Just r, cfrFinished f)

    failed = composeFailed ctx >>= \case
        Nothing -> return (Nothing, [])
        Just r  -> case decodeComposeFailedResponse r of
            Nothing -> return (Just r, [])
            Just f  -> return (Just r, cfrFailed f)

composeCommand ctx ("delete":xs) = composeDelete ctx (intercalate "," xs) >>= \case
    Nothing -> putStrLn "ERROR: No server response"
    Just r  -> do
        j <- asValue r
        if isJSONOutput ctx
            then putStrLn $ prettyJson $ j ^. responseBody
            else case decodeComposeDeleteResponse r of
                Nothing    -> putStrLn "ERROR: Problem decoding response"
                Just uuids -> do
                    mapM_ printUuidStatus $ cdrUuids uuids
                    mapM_ printUuidError $ cdrErrors uuids
  where
    statusString :: Bool -> String
    statusString True  = "Ok"
    statusString False = "Failed"

    printUuidStatus UuidStatus{..} = printf "%s: %s\n" usUuid (statusString usStatus)
    printUuidError UuidError{..} = printf "%s: ERROR - %s\n" ueUuid ueMsg

composeCommand _    _      = putStrLn "ERROR: Unknown compose type"


-- | Process the recipes list command
-- Prints a JSON or human reable list of available recipes
recipesCommand :: CommandCtx -> [String] -> IO ()
recipesCommand ctx ("list":_) = listRecipes ctx >>= \case
    Nothing -> putStrLn "ERROR: No server response"
    Just r  -> do
        j <- asValue r
        if isJSONOutput ctx
            then putStrLn $ prettyJson $ j ^. responseBody
            else putStrLn $ "Blueprints: " ++ humanRecipesList j

-- | Process the recipes show command
-- Print the TOML recipe
recipesCommand ctx ("show":xs) = mapM_ showRecipe $ argify xs
  where
    showRecipe x = infoRecipes ctx x >>= \r ->
        -- TODO This needs to check for JSON output selection...
        putStrLn $ C8.unpack $ fromJust r ^. responseBody

-- | Process the recipes save command
-- Save a copy of the recipe to a TOML file using <recipe name>.toml
recipesCommand ctx ("save":xs) = mapM_ saveRecipe $ argify xs
  where
    saveRecipe x = infoRecipes ctx x >>= \r ->
        -- TODO This needs to check for JSON output selection and save it as a .json file instead
        writeFile (tomlFileName x) $ C8.unpack $ fromJust r ^. responseBody

-- | Process the recipe depsolve command
-- Print the list of package versions needed for the recipe list
recipesCommand ctx ("depsolve":xs) = depsolveRecipes ctx (intercalate "," xs) >>= \case
    Nothing -> putStrLn "ERROR: No server response"
    Just r  -> do
        j <- asValue r
        if isJSONOutput ctx
            then putStrLn $ prettyJson $ j ^. responseBody
            else do
                let deps = decodeDepsolve r
                if isJust deps
                    then putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ recipesDepsList $ fromJust deps
                    else putStrLn "ERROR: Cannot decode JSON response"

-- | Process the recipes push command
-- Create a new recipe on the server, or overwrite an existing one, with a TOML recipe file
recipesCommand ctx ("push":xs) = mapM_ pushRecipe $ argify xs
  where
    pushRecipe name = do
        unlessM (doesFileExist name) $ do
            putStrLn $ printf "ERROR: Missing file %s" name
            exitFailure
        toml <- readFile name
        newRecipes ctx toml

-- | recipes workspace
-- Write the recipe(s) to the server's temporary workspace storage
recipesCommand ctx ("workspace":xs) = mapM_ pushRecipe $ argify xs
  where
    pushRecipe name = do
        unlessM (doesFileExist name) $ do
            putStrLn $ printf "ERROR: Missing file %s" name
            exitFailure
        toml <- readFile name
        workspaceRecipes ctx toml >>= \r -> handleAPIResponse ctx r

-- | recipes delete <recipe-name>
-- Delete a recipe from the server
recipesCommand _ ["delete"]            = putStrLn "ERROR: missing blueprint name"
recipesCommand ctx ("delete":recipe:_) =
    deleteRecipe ctx recipe >>= \r -> handleAPIResponse ctx r

-- | recipes tag <recipe-name>
-- Tag the most recent recipe commit as a release
recipesCommand _ ["tag"]            = putStrLn "ERROR: missing blueprint name"
recipesCommand ctx ("tag":recipe:_) =
    tagRecipe ctx recipe >>= \r -> handleAPIResponse ctx r

-- | recipes changes <recipe-name>
-- Show the changes to the selected recipes
-- TODO How to support offset and limit? Defaults to 0, 20
recipesCommand _ ["changes"]      = putStrLn "ERROR: missing blueprint name(s)"
recipesCommand ctx ("changes":xs) = changesRecipes ctx (intercalate "," xs) >>= \r -> do
    j <- asValue $ fromJust r

    printJSON ctx j
    printChanges $ response r
    printErrors ctx $ errors r
  where
    response r = fromJust $ decodeRecipesChangesResponse $ fromJust r
    errors r = rcrErrors $ response r
    printChanges resp = unless (isJSONOutput ctx) $ mapM_ putStrLn $ prettyRecipeChanges $ rcrRecipes resp

-- | recipe undo <recipe-name> <commit>
-- Revert a recipe to a commit
recipesCommand _ ["undo"]                   = putStrLn "ERROR: Missing blueprint-name and commit hash"
recipesCommand _ ["undo", _]                = putStrLn "ERROR: Missing commit hash"
recipesCommand ctx ("undo":recipe:commit:_) =
    undoRecipe ctx recipe commit >>= \r -> handleAPIResponse ctx r

-- | recipe diff <recipe-name> <from-commit> <to-commit>
-- Show the differences between 2 versions of the recipe
recipesCommand _ ["diff"]                     = putStrLn "ERROR: Missing blueprint-name, from-commit, and to-commit"
recipesCommand _ ["diff", _]                  = putStrLn "ERROR: Missing from-commit, and to-commit"
recipesCommand _ ["diff", _, _]               = putStrLn "ERROR: Missing to-commit"
recipesCommand ctx ("diff":recipe:from:to:_)  = diffRecipe ctx recipe from to >>= \r -> do
    j <- asValue $ fromJust r

    printJSON ctx j
    printDiff $ response r
  where
    response r = fromJust $ decodeRecipesDiffResponse $ fromJust r
    printDiff resp = unless (isJSONOutput ctx) $ mapM_ putStrLn $ prettyRecipeDiff $ rdrDiff resp

recipesCommand _    (x:_) = putStrLn $ printf "ERROR: Unknown blueprints command - %s" x
recipesCommand _    _     = putStrLn "ERROR: Missing blueprints command"

-- | Process the recipes freeze show command
-- Show the frozen recipe in TOML format
recipesFreeze :: CommandCtx -> [String] -> IO ()
recipesFreeze ctx ("show":xs) = mapM_ showFrozenRecipe $ argify xs
  where
    showFrozenRecipe x = freezeRecipeToml ctx x >>= \r -> 
        putStrLn $ C8.unpack $ fromJust r ^. responseBody

-- | Process the recipes freeze show command
-- Save the frozen recipe in TOML format, as <recipe name>.frozen.toml
recipesFreeze ctx ("save":xs) = mapM_ saveFrozenRecipe $ argify xs
  where
    saveFrozenRecipe x = freezeRecipeToml ctx x >>= \r ->
        writeFile (frozenTomlFileName x) $ C8.unpack $ fromJust r ^. responseBody

-- | Process the recipes freeze
-- Display the recipes' frozen module and packages list in human readable format
recipesFreeze ctx xs = freezeRecipes ctx (intercalate "," xs) >>= \case
    Nothing -> putStrLn "ERROR: No server response"
    Just r  -> do
        j <- asValue r
        if isJSONOutput ctx
            then putStrLn $ prettyJson $ j ^. responseBody
            else do
                let recipes = decodeFreeze r
                if isJust recipes
                    then putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ recipesFrozenList $ fromJust recipes
                    else putStrLn "ERROR: Cannot decode JSON response"


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
parseCommand ctx ("compose":xs)             = composeCommand ctx xs
parseCommand ctx ("blueprints":"freeze":xs) = recipesFreeze ctx xs
parseCommand ctx ("blueprints":xs)          = recipesCommand ctx xs
parseCommand ctx ("modules":xs)             = modulesCommand ctx xs
parseCommand ctx ("projects":xs)            = projectsCommand ctx xs
parseCommand _   ("help":xs)                = helpCommand xs
parseCommand _   []                         = helpCommand []
parseCommand _   _                          = putStrLn "Unknown Command"
