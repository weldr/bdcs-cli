-- Copyright (C) 2017 Red Hat, Inc.
--
-- This file is part of bdcs-api-server.
--
-- bdcs-api-server is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- bdcs-api-server is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with bdcs-api-server.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Conditional (unlessM)
import qualified Control.Exception as E
import Control.Lens ((^..), (^.), (&), (.~))
import Control.Monad (liftM, when, mzero)
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
import System.Console.GetOpt
import System.Directory(doesFileExist)
import System.Environment (getArgs)
import System.Exit(exitFailure)
import System.IO
import Text.Printf(printf)




-- | Join a list of strings with a delimiter.
join delim xs = concat (intersperse delim xs)

-- | Take a list of possiby comma, or comma-space, separated options and turn it into a list of options
argify xs = filter (/= "") $ concatMap (splitOn ",") xs

-- | Return the TOML filename, ending with .toml
tomlFileName :: String -> FilePath
tomlFileName = printf "%s.toml"

-- | Convert the Value into a pretty JSON string for printing.
prettyJson :: Value -> String
prettyJson json = C8.unpack $ encodePretty json

-- | Turn exceptions from an action into Nothing
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = E.handle (\(e::E.SomeException) -> (return Nothing)) (Just `liftM` act)


--
-- Commandline parsing
--

data CliOptions = CliOptions
    { optVerbose     :: Bool
    , optShowVersion :: Bool
    , optJsonOutput  :: Bool
    , optUrl         :: String
    , optApi         :: String
    } deriving Show

defaultOptions    = CliOptions
    { optVerbose     = False
    , optShowVersion = False
    , optJsonOutput  = False
    , optUrl         = "http://localhost:4000/"
    , optApi         = "0"
    }

cliOptions :: [OptDescr (CliOptions -> CliOptions)]
cliOptions =
    [ Option ['v']     ["verbose"]
        (NoArg (\opts -> opts { optVerbose = True }))
        "Verbose output"
    , Option ['V','?'] ["version"]
        (NoArg (\opts -> opts { optShowVersion = True }))
        "show version number"
    , Option ['j'] ["json"]
        (NoArg (\opts -> opts { optJsonOutput = True }))
        "Show results as JSON objects"
    , Option ['u']     ["url"]
        (ReqArg (\url opts -> opts { optUrl = url }) "URL")
        "URL to use for the API requests"
    , Option ['a']     ["api"]
        (ReqArg (\api opts -> opts { optApi = api }) "API")
        "URL to use for the API requests"
    ]
cliHeader = "Usage: bdcs-cli [OPTIONS...] commands..."

parseOpts :: [String] -> IO (CliOptions, [String])
parseOpts argv =
    case getOpt Permute cliOptions argv of
        (o,n,[]  ) -> return (foldl (flip id) Main.defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo cliHeader cliOptions))


helpText :: String
helpText = "\
\  recipes list                  List the names of the available recipes.\n\
\          show <recipe,...>     Display the recipe in TOML format.\n\
\          save <recipe,...>     Save the recipe to a file, <recipe-name>.toml\n\
\          depsolve <recipe,...> Display the packages needed to install the recipe.\n\
\          push <recipe>         Push a recipe TOML file to the server.\n\
\  modules list                  List the available modules.\n\
\  projects list                 List the available projects.\n\
\\n"

helpCommand :: [String] -> IO ()
helpCommand _ = do
    putStrLn $ usageInfo cliHeader cliOptions
    putStr helpText


-- | Print the API URL selection (or the default)
printUrl :: CliOptions -> IO ()
printUrl CliOptions{..} = putStrLn optUrl

-- | Construct an API URL based on cmdline options or defaults.
api_url :: CliOptions -> String -> String
api_url CliOptions{..} route = optUrl ++ "api/v" ++ optApi ++ "/" ++ route

-- | Fetch data from a URL and ignore errors by returning Nothing, or a Lazy ByteString
getUrl :: Session -> String -> IO (Maybe (Response BSL.ByteString))
getUrl sess url = maybeIO (S.get sess url)

-- | Post a String to a URL and return the Response from the server, or Nothing
postUrl :: Session -> String -> String -> IO (Maybe (Response BSL.ByteString))
postUrl sess url bodyStr = do
    let opts = defaults & header "Content-Type" .~ ["text/x-toml"]
    let bodyBytes = C8.pack bodyStr
    maybeIO (S.postWith opts sess url bodyBytes)


-- | Request the list of recipes from the API server
listRecipes :: Session -> CliOptions -> IO (Maybe (Response BSL.ByteString))
listRecipes sess opts = getUrl sess $ api_url opts "recipes/list"

-- | Request the TOML copy of the Recipe from the API server
infoRecipes :: Session -> CliOptions -> String -> IO (Maybe (Response BSL.ByteString))
infoRecipes sess opts recipe = getUrl sess $ api_url opts "recipes/info/" ++ recipe ++ "?format=toml"

-- | Request the dependecies for the recipe from the APO server
depsolveRecipes :: Session -> CliOptions -> String -> IO (Maybe (Response BSL.ByteString))
depsolveRecipes sess opts recipes = getUrl sess $ api_url opts "recipes/depsolve/" ++ recipes

-- | POST a new TOML recipe to the API server
newRecipes :: Session -> CliOptions -> String -> IO (Maybe (Response BSL.ByteString))
newRecipes sess opts bodyStr = postUrl sess (api_url opts "recipes/new") bodyStr

-- | Request a list of the available modules from the API server
listModules :: Session -> CliOptions -> IO (Maybe (Response BSL.ByteString))
listModules sess opts = getUrl sess $ api_url opts "modules/list"

-- | Request a list of the available projects from the API server
listProjects :: Session -> CliOptions -> IO (Maybe (Response BSL.ByteString))
listProjects sess opts = getUrl sess $ api_url opts "projects/list"

-- | Request detailed info for a list of projects
infoProjects :: Session -> CliOptions -> String -> IO (Maybe (Response BSL.ByteString))
infoProjects sess opts projects = getUrl sess $ api_url opts "projects/info/" ++ projects


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

-- | Get the Recipe from the depsolve results
getRecipe :: RecipeDeps -> Recipe
getRecipe RecipeDeps{..} = rdRecipe

-- | Get the module list from the depsolve
-- includes the exact versions of the recipe's modules and packages
getModules :: RecipeDeps -> [PackageNEVRA]
getModules RecipeDeps{..} = rdModules

-- | Get all the dependencies needed for the recipe
getDependencies :: RecipeDeps -> [PackageNEVRA]
getDependencies RecipeDeps{..} = rdDependencies

-- | Get the depsolved recipes from the request results
getRecipes :: DependencyJSON -> [RecipeDeps]
getRecipes DependencyJSON{..} = djRecipes


-- | Convert the server response into data structures
decodeDepsolve :: Response C8.ByteString -> Maybe DependencyJSON
decodeDepsolve resp = decode $ resp ^. responseBody

-- | Extract the names and versions of the recipes in the depsolve response
recipesList :: DependencyJSON -> [String]
recipesList deps = map (recipeNameVersion . getRecipe) $ getRecipes deps

-- | Extract the dependency list from the depsolve response
dependenciesList :: RecipeDeps -> [String]
dependenciesList recipeDeps = map (\p -> "    " ++ packageNEVRA p) $ getDependencies recipeDeps

-- | Return the dependencies for each recipe in the depsolve response
recipesDepsList :: DependencyJSON -> [[String]]
recipesDepsList deps = map recipeDetails $ getRecipes deps
  where
    recipeDetails recipeDeps = [name recipeDeps] ++ dependenciesList recipeDeps
    name = recipeNameVersion . getRecipe


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
parseCommand sess opts ("recipes":xs)      = recipesCommand sess opts xs
parseCommand sess opts ("modules":xs)      = modulesCommand sess opts xs
parseCommand sess opts ("projects":xs)     = projectsCommand sess opts xs
parseCommand sess opts ("help":xs)         = helpCommand xs
parseCommand _    _    _                   = putStrLn "Unknown Command"

main :: IO ()
main = S.withSession $ \sess -> do
    args <- getArgs
    r <- parseOpts args
    let opts = fst r
    let commands = snd r
    when (optVerbose opts) $ do
        print opts
        print commands
        printUrl opts
    parseCommand sess opts commands
