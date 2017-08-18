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
{-# LANGUAGE RecordWildCards   #-}

module BDCSCli.Recipe(parseRecipe,
                      doGitTests)
  where

import           Control.Conditional(ifM)
import           Data.Aeson(toJSON, fromJSON)
import           Data.Aeson.Types(Result(..))
import qualified Data.ByteString as BS
import           Data.Maybe(fromJust, fromMaybe, isJust)
import qualified Data.Text as T
import           Data.Word(Word32)
import           GI.Gio
import qualified GI.Ggit as Git
import           System.Directory(doesPathExist)
import           Text.Printf(printf)
import           Text.Toml(parseTomlDoc)

import           BDCSCli.API.V0(Recipe(..), RecipeModule(..))

-- | Parse a TOML formatted string and return a Recipe
parseRecipe :: T.Text -> Either String Recipe
parseRecipe xs =
    case parseTomlDoc "" xs of
        Left err    -> Left ("Parsing TOML document failed. " ++ show err)
        Right table -> do
            let json = toJSON table
            case (fromJSON json :: Result Recipe) of
                Error err -> Left ("Converting from JSON to Recipe failed. " ++ show err)
                Success r -> Right r

-- | Convert a Recipe to a TOML string
recipeTOML :: Recipe -> T.Text
recipeTOML Recipe{..} = T.concat [nameText, versionText, descriptionText, modulesText, packagesText]
  where
    nameText = T.pack $ printf "name = \"%s\"\n" rName
    versionText = T.pack $ printf "version = \"%s\"\n" $ fromMaybe "" rVersion
    descriptionText = T.pack $ printf "description = \"%s\"\n\n" rDescription

    moduleText :: T.Text -> RecipeModule -> T.Text
    moduleText name RecipeModule{..} = T.pack $ printf "[[%s]]\nname = \"%s\"\nversion = \"%s\"\n\n" name rmName rmVersion
    packagesText = T.concat $ map (moduleText "packages") rPackages
    modulesText = T.concat $ map (moduleText "modules") rModules

-- | Open a Git repository, or create the initial repository if one doesn't exist
openOrCreateRepo :: FilePath -> IO Git.Repository
openOrCreateRepo path = do
    gfile <- fileNewForPath path
    ifM (doesPathExist path)
        (Git.repositoryOpen gfile)
        (createWithInitialCommit gfile)
  where
    createWithInitialCommit gfile = do
        repo <- Git.repositoryInitRepository gfile True

        -- Make an empty initial commit
        sig <- Git.signatureNewNow "bdcs-cli" "user-email"
        index <- Git.repositoryGetIndex repo
        tree_id <- Git.indexWriteTree index
        tree <- fromJust <$> Git.repositoryLookupTree repo tree_id
        let ref = Just "HEAD"
        let encoding = Just "UTF-8"
        commit_id <- Git.repositoryCreateCommit repo ref sig sig encoding "Initial Recipe repository commit" tree []

        return repo

-- | Lookup the Branch name or create a new branch and return a Git.Branch
findOrCreateBranch :: Git.Repository -> T.Text -> IO Git.Branch
findOrCreateBranch repo branch = do
    mbranch <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal
    if isJust mbranch
    then return $ fromJust mbranch
    else createBranch repo branch
  where
    createBranch repo branch = do
        head_ref <- Git.repositoryGetHead repo
        parent_obj <- Git.refLookup head_ref
        mbranch <- Git.repositoryCreateBranch repo branch parent_obj [Git.CreateFlagsNone]
        -- XXX Handle errors
        return $ fromJust mbranch

-- | Convert a Branch object to an OId
-- XXX Ignored error handling for the moment.
getBranchOIdFromObject :: Git.Repository -> Git.Branch -> IO Git.OId
getBranchOIdFromObject repo branch_obj = do
    branch_name <- Git.branchGetName branch_obj
    let branch_ref = T.pack $ printf "refs/heads/%s" branch_name
    ref <- Git.repositoryLookupReference repo branch_ref
    Git.refGetTarget ref

-- | Make a new commit to a repository's branch
-- XXX Ignored error handling for the moment.
writeCommit :: Git.Repository -> T.Text -> T.Text -> T.Text -> BS.ByteString -> IO Git.OId
writeCommit repo branch filename message content = do
    -- does the branch exist? If so get its OId: repositoryLookupBranch
    -- If it does not, create it and get its OId: repositoryCreateBranch
    branch_obj <- findOrCreateBranch repo branch
    branch_id <- getBranchOIdFromObject repo branch_obj

    -- get the parent commit for this branch: repositoryLookupCommit
    mparent_commit <- Git.repositoryLookupCommit repo branch_id
    -- XXX Handle errors
    let parent_commit = fromJust mparent_commit

    -- create a blob for content: repositoryCreateBlobFromBuffer
    blob_id <- Git.repositoryCreateBlobFromBuffer repo content

    -- Use treebuilder to make a new entry for this filename and blob: repositoryCreateTreeBuilderFromTree
    mparent_tree <- Git.commitGetTree parent_commit
    -- XXX Handle errors
    let parent_tree = fromJust mparent_tree
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree
    -- treeBuilderInsert, treeBuilderWrite
    Git.treeBuilderInsert builder filename blob_id Git.FileModeBlob
    tree_id <- Git.treeBuilderWrite builder
    -- XXX Handle errors
    mtree <- Git.repositoryLookupTree repo tree_id
    let tree = fromJust mtree

    -- Create a signature
    sig <- Git.signatureNewNow "bdcs-cli" "user-email"

    let ref = Just $ T.pack $ printf "refs/heads/%s" branch
    let encoding = Just "UTF-8"

    -- Create a new commit: repositoryCreateCommit
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit]

-- | Read a commit and return a ByteString of the content
-- TODO Return the commit message too
readCommit :: Git.Repository -> T.Text -> T.Text -> Maybe T.Text -> IO BS.ByteString
readCommit repo branch filename Nothing = do
    let spec = T.pack $ printf "%s:%s" branch filename
    readCommitSpec repo spec
readCommit repo branch filename commit = do
    let spec = T.pack $ printf "%s:%s" (fromJust $ commit) filename
    readCommitSpec repo spec

-- | Read a commit usinga revspec, return the ByteString content
readCommitSpec :: Git.Repository -> T.Text -> IO BS.ByteString
readCommitSpec repo spec = do
    obj <- Git.repositoryRevparse repo spec
    oid <- Git.objectGetId obj
    -- XXX Handle errors
    mblob <- Git.repositoryLookupBlob repo oid
    let blob = fromJust mblob
    Git.blobGetRawContent blob

-- | Get the filename for a Blob tree entry
getFilename :: Git.Tree -> Word32 -> IO (Maybe T.Text)
getFilename tree idx = do
    mentry <- Git.treeGet tree idx
    -- XXX Handle errors
    let entry = fromJust mentry

    -- Only allow Blob and BlobExecutable
    isBlob <- isFileBlob entry
    case isBlob of
        False   -> return Nothing
        True    -> do
            mfilename <- Git.treeEntryGetName entry
            -- XXX Handle errors
            let name = fromJust mfilename
            return $ Just name
 where
    isFileBlob entry = do
        mode <- Git.treeEntryGetFileMode entry
        case mode of
            Git.FileModeBlob           -> return True
            Git.FileModeBlobExecutable -> return True
            _                          -> return False

-- | Get a list of the Blob tree entry filenames
getFilenames :: Git.Tree -> Word32 -> IO [T.Text]
getFilenames tree idx = getFilenames' tree [] idx

-- | Build the list of filenames from the tree entries
getFilenames' :: Git.Tree -> [T.Text] -> Word32 -> IO [T.Text]
getFilenames' _ filenames 0 = return filenames
getFilenames' tree filenames idx = do
    filename <- getFilename tree (idx-1)
    case filename of
        Just name -> getFilenames' tree (name:filenames) (idx-1)
        Nothing   -> getFilenames' tree filenames (idx-1)

-- | List the files on a branch
listBranchFiles :: Git.Repository -> T.Text -> Maybe T.Text -> IO [T.Text]
listBranchFiles repo branch Nothing = do
    mbranch <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal
    -- XXX Handle errors
    let branch_obj = fromJust mbranch
    branch_id <- getBranchOIdFromObject repo branch_obj
    -- get the parent commit for this branch: repositoryLookupCommit
    mparent_commit <- Git.repositoryLookupCommit repo branch_id
    -- XXX Handle errors
    let parent_commit = fromJust mparent_commit
    listCommitFiles repo parent_commit

-- | List the files in a commit
listCommitFiles :: Git.Repository -> Git.Commit -> IO [T.Text]
listCommitFiles repo commit = do
    mparent_tree_id <- Git.commitGetTreeId commit
    let parent_tree_id = fromJust mparent_tree_id

    mtree <- Git.repositoryLookupTree repo parent_tree_id
    -- XXX Handle errors
    let tree = fromJust mtree

    sz <- Git.treeSize tree
    getFilenames tree sz

deleteFile :: Git.Repository -> T.Text -> T.Text -> IO ()
deleteFile repo branch filename = do
    mbranch <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal
    -- XXX Handle errors
    let branch_obj = fromJust mbranch
    branch_id <- getBranchOIdFromObject repo branch_obj
    -- get the parent commit for this branch: repositoryLookupCommit
    mparent_commit <- Git.repositoryLookupCommit repo branch_id
    -- XXX Handle errors
    let parent_commit = fromJust mparent_commit

    -- Use treebuilder to modify the tree
    mparent_tree <- Git.commitGetTree parent_commit
    -- XXX Handle errors
    let parent_tree = fromJust mparent_tree
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree
    -- treeBuilderInsert, treeBuilderWrite
    Git.treeBuilderRemove builder filename
    tree_id <- Git.treeBuilderWrite builder
    -- XXX Handle errors
    mtree <- Git.repositoryLookupTree repo tree_id
    let tree = fromJust mtree

    -- Create a signature
    sig <- Git.signatureNewNow "bdcs-cli" "user-email"

    let ref = Just $ T.pack $ printf "refs/heads/%s" branch
    let encoding = Just "UTF-8"

    let message = T.pack $ printf "Recipe %s deleted" filename

    -- Create a new commit: repositoryCreateCommit
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit]
    return ()



-- | Test out git functions
doGitTests :: FilePath -> IO ()
doGitTests path = do
    -- Move this elsewhere later, MUST be called first
    Git.init
    repo <- openOrCreateRepo path
-- WORKS
--    commit_id <- writeCommit repo "master" "README" "A test commit\n\nWith some commentary." "Some Content"
--    commit_id <- writeCommit repo "master" "README" "A test commit 2\n\nWith some other commentary." "Some Content #2"
--    commit_id <- writeCommit repo "master" "TODO" "A todo commit\n\nWith some stuff to do." "Some Content"
--    commit_id <- writeCommit repo "master" "TODO" "A todo commit\n\nWith some commentary." "Some other Content"

    files <- listBranchFiles repo "master" Nothing
    print files

    commit_id <- writeCommit repo "master" "FRODO" "A list of stuff to be accomplished\n\nFind a magic ring" "Some other Content"
    files <- listBranchFiles repo "master" Nothing
    print files

    deleteFile repo "master" "FRODO"

    files <- listBranchFiles repo "master" Nothing
    print files

    return ()
