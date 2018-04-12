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

import           Data.List(dropWhileEnd)
import           System.Console.GetOpt
import           System.Environment(getArgs)

--
-- Commandline parsing
--

data CliOptions = CliOptions
    { optVerbose     :: Bool
    , optShowVersion :: Bool
    , optJsonOutput  :: Bool
    , optUrl         :: String
    , optApi         :: String
    , optMDDB        :: String
    , optRepo        :: String
    } deriving Show

defaultOptions :: CliOptions
defaultOptions    = CliOptions
    { optVerbose     = False
    , optShowVersion = False
    , optJsonOutput  = False
    , optUrl         = "http://localhost:4000"
    , optApi         = "0"
    , optMDDB        = "/var/tmp/mddb/metadata.db"
    , optRepo        = "/var/tmp/repo/"
    }

-- | Drop any trailing slashes from the string
dropSlash :: String -> String
dropSlash = dropWhileEnd (== '/')

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
        (ReqArg (\url opts -> opts { optUrl = dropSlash url }) "URL")
        "URL to use for the API requests"
    , Option ['a']     ["api"]
        (ReqArg (\api opts -> opts { optApi = api }) "API")
        "API version to use for the API requests"
    , Option ['m']     ["mddb"]
        (ReqArg (\mddb opts -> opts { optMDDB = mddb }) "MDDB")
        "Path to metadata.db, used by compose tar command"
    , Option ['r']     ["repo"]
        (ReqArg (\repo opts -> opts { optRepo = repo }) "REPO")
        "Path to BDCS Repo, used by compose tar command"
    ]

cliHeader :: String
cliHeader = "Usage: bdcs-cli [OPTIONS...] commands..."

parseOpts :: [String] -> IO (CliOptions, [String])
parseOpts argv =
    case getOpt Permute cliOptions argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo cliHeader cliOptions))


helpText :: String
helpText = "\
\  compose    start <blueprint> <type>    Start a compose using the selected blueprint and output type.\n\
\             types                       List the supported output types.\n\
\             status                      List the status of all running and finished composes.\n\
\             log <uuid> [<size>kB]       Show the last 1kB of the compose log.\n\
\             cancel <uuid>               Cancel a running compose and delete any intermediate results.\n\
\             delete <uuid,...>           Delete the listed compose results.\n\
\             info <uuid>                 Show detailed information on the compose.\n\
\             metadata <uuid>             Download the metadata use to create the compose to <uuid>-metadata.tar\n\
\             logs <uuid>                 Download the compose logs to <uuid>-logs.tar\n\
\             results <uuid>              Download all of the compose results; metadata, logs, and image to <uuid>.tar\n\
\             image <uuid>                Download the output image from the compose. Filename depends on the type.\n\
\  blueprints list                  List the names of the available blueprints.\n\
\             show <blueprint,...>        Display the blueprint in TOML format.\n\
\             changes <blueprint,...>     Display the changes for each blueprint.\n\
\             diff <blueprint-name>       Display the differences between 2 versions of a blueprint.\n\
\                  <from-commit>          Commit hash or NEWEST\n\
\                  <to-commit>            Commit hash, NEWEST, or WORKSPACE\n\
\             save <blueprint,...>        Save the blueprint to a file, <blueprint-name>.toml\n\
\             delete <blueprint>          Delete a blueprint from the server\n\
\             depsolve <blueprint,...>    Display the packages needed to install the blueprint.\n\
\             push <blueprint>            Push a blueprint TOML file to the server.\n\
\             freeze <blueprint,...>      Display the frozen blueprint's modules and packages.\n\
\             freeze show <blueprint,...> Display the frozen blueprint in TOML format.\n\
\             freeze save <blueprint,...> Save the frozen blueprint to a file, <blueprint-name>.frozen.toml.\n\
\             tag <blueprint>             Tag the most recent blueprint commit as a release.\n\
\             undo <blueprint> <commit>   Undo changes to a blueprint by reverting to the selected commit.\n\
\             workspace <blueprint>       Push the blueprint TOML to the temporary workspace storage.\n\
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
