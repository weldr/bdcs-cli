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
module BDCSCli.Cmdline(CliOptions(..),
                       parseArgs,
                       helpCommand)
  where

import System.Console.GetOpt
import System.Environment (getArgs)

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
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo cliHeader cliOptions))


helpText :: String
helpText = "\
\  recipes list                     List the names of the available recipes.\n\
\          show <recipe,...>        Display the recipe in TOML format.\n\
\          save <recipe,...>        Save the recipe to a file, <recipe-name>.toml\n\
\          depsolve <recipe,...>    Display the packages needed to install the recipe.\n\
\          push <recipe>            Push a recipe TOML file to the server.\n\
\          freeze <recipe,...>      Display the frozen recipe's modules and packages.\n\
\          freeze show <recipe,...> Display the frozen recipe in TOML format.\n\
\          freeze save <recipe,...> Save the frozen recipe to a file, <recipe-name>.frozen.toml.\n\
\  modules list                     List the available modules.\n\
\  projects list                    List the available projects.\n\
\\n"

helpCommand :: [String] -> IO ()
helpCommand _ = do
    putStrLn $ usageInfo cliHeader cliOptions
    putStr helpText

parseArgs :: IO (CliOptions, [String])
parseArgs = do
    args <- getArgs
    parseOpts args
