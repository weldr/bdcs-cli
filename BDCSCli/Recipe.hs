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
{-# LANGUAGE RecordWildCards   #-}

module BDCSCli.Recipe(parseRecipe,
                      recipeTOML,
                      recipeTomlFilename,
                      recipeBumpVersion,
                      openOrCreateRepo,
                      findOrCreateBranch,
                      getBranchOIdFromObject,
                      writeCommit,
                      readCommit,
                      readCommitSpec,
                      listBranchFiles,
                      listCommitFiles,
                      deleteFile,
                      revertFile,
                      revertFileCommit,
                      listCommits,
                      findCommitTag,
                      getRevisionFromTag,
                      tagFileCommit,
                      commitRecipeFile,
                      commitRecipe,
                      commitRecipeDirectory,
                      Recipe(..),
                      RecipeModule(..),
                      runGitRepoTests)
  where

import           Control.Conditional(ifM)
import           Control.Exception
import           Control.Monad(filterM, unless)
import           Control.Monad.Loops(allM)
import           Data.Aeson(toJSON, fromJSON)
import           Data.Aeson.Types(Result(..))
import qualified Data.ByteString as BS
import           Data.Either(rights)
import           Data.List(elemIndices, isSuffixOf)
import           Data.Maybe(fromJust, fromMaybe, isJust)
import qualified Data.SemVer as SV
import qualified Data.Text as T
import           Data.Text.Encoding(encodeUtf8)
import           Data.Word(Word32)
import           GI.Gio
import qualified GI.Ggit as Git
import qualified GI.GLib as GLib
import           System.Directory(doesPathExist, listDirectory)
import           System.IO.Temp(withTempDirectory)
import           Text.Printf(printf)
import           Text.Read(readMaybe)
import           Text.Toml(parseTomlDoc)

import           BDCSCli.API.V0(Recipe(..), RecipeModule(..))
import           BDCSCli.Utilities(maybeThrow)

data GitError =
    OpenRepoError
  | CreateRepoError
  | CreateBlobError
  | CreateCommitError
  | CreateBranchError
  | BranchNameError
  | WriteTreeError
  | GetIndexError
  | GetHeadError
  | RefLookupError
  | TreeBuilderError
  | GetByNameError
  | GetNameError
  | GetTargetError
  | GetTimeError
  | GetTimeZoneError
  | GetTreeError
  | GetTreeIdError
  | GetCommitterError
  | GetMessageError
  | GetParentsError
  | LookupError
  | LookupBlobError
  | LookupBranchError
  | LookupCommitError
  | LookupTagError
  | LookupTreeError
  | LookupReferenceError
  | RevparseError
  | BuilderWriteError
  | BuilderInsertError
  | GetEntryIdError
  | GetIdError
  | GetRawBlobError
  | GetTargetIdError
  | NewOIdError
  | NewOptionsError
  | NewTimeValError
  | NewTreeError
  | NewSignatureError
  | NewWalkerError
  | OIdError
  deriving (Eq, Show)

instance Exception GitError


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

-- | Convert a recipe name to a toml filename
--
-- Replaces spaces with - and append .toml
recipeTomlFilename :: String -> T.Text
recipeTomlFilename name = T.append (T.replace " " "-" (T.pack name)) ".toml"

-- | semver recipe version number bump
--
-- If neither have a version 0.0.1 is returned
-- If there is no previous version the new version is checked and returned
-- If there is no new version, but there is a previous one, bump its patch level
-- If the previous and new versions are the same, bump the patch level
-- If they are different, check and return the new version
recipeBumpVersion :: Maybe String -> Maybe String -> Either String String
recipeBumpVersion Nothing Nothing = Right "0.0.1"
-- Only a new version, make sure the new version can be parsed
recipeBumpVersion Nothing (Just new_ver) =
    case SV.fromText (T.pack new_ver) of
        Right _ -> Right new_ver
        Left  _ -> Left ("Failed to parse version: " ++ new_ver)
-- If there was a previous version and no new one, bump the patch level
recipeBumpVersion (Just prev_ver) Nothing =
    case SV.fromText (T.pack prev_ver) of
        Right version -> Right $ SV.toString $ SV.incrementPatch version
        Left _        -> Left ("Failed to parse version: " ++ prev_ver)
recipeBumpVersion (Just prev_ver) (Just new_ver)
    | prev_ver == new_ver = bumpNewVer
    | otherwise           = checkNewVer
  where
    bumpNewVer =
        case SV.fromText (T.pack new_ver) of
            Right version -> Right $ SV.toString $ SV.incrementPatch version
            Left _        -> Left ("Failed to parse version: " ++ new_ver)
    checkNewVer =
        case SV.fromText (T.pack new_ver) of
            Right _ -> Right new_ver
            Left  _ -> Left ("Failed to parse version: " ++ new_ver)

-- | Get the branch's HEAD Commit
headCommit :: Git.Repository -> T.Text -> IO Git.Commit
headCommit repo branch = do
    branch_obj <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal >>= maybeThrow LookupBranchError
    branch_id <- getBranchOIdFromObject repo branch_obj
    Git.repositoryLookupCommit repo branch_id >>= maybeThrow LookupCommitError

-- | Prepare for a commit
prepareCommit :: Git.Repository -> T.Text -> Git.TreeBuilder -> IO (Git.Tree, Git.Signature, Maybe T.Text, Maybe T.Text)
prepareCommit repo branch builder = do
    tree_id <- Git.treeBuilderWrite builder >>= maybeThrow BuilderWriteError
    tree <- Git.repositoryLookupTree repo tree_id >>= maybeThrow LookupTreeError
    sig <- Git.signatureNewNow "bdcs-cli" "user-email" >>= maybeThrow NewSignatureError
    let ref = Just $ T.pack $ printf "refs/heads/%s" branch
    let encoding = Just "UTF-8"
    return (tree, sig, ref, encoding)

-- | Open a Git repository, or create the initial repository if one doesn't exist
openOrCreateRepo :: FilePath -> IO Git.Repository
openOrCreateRepo path = do
    gfile <- fileNewForPath path
    ifM (doesPathExist $ path ++ "/.git")
        (openRepo gfile)
        (createWithInitialCommit gfile)
  where
    openRepo gfile = Git.repositoryOpen gfile >>= maybeThrow OpenRepoError

    createWithInitialCommit gfile = do
        repo <- Git.repositoryInitRepository gfile True >>= maybeThrow CreateRepoError

        -- Make an empty initial commit
        sig <- Git.signatureNewNow "bdcs-cli" "user-email" >>= maybeThrow NewSignatureError
        index <- Git.repositoryGetIndex repo >>= maybeThrow GetIndexError
        tree_id <- Git.indexWriteTree index >>= maybeThrow WriteTreeError
        tree <- Git.repositoryLookupTree repo tree_id >>= maybeThrow LookupTreeError
        let ref = Just "HEAD"
        let encoding = Just "UTF-8"
        Git.repositoryCreateCommit repo ref sig sig encoding "Initial Recipe repository commit" tree [] >>= maybeThrow CreateCommitError

        return repo

-- | Lookup the Branch name or create a new branch and return a Git.Branch
findOrCreateBranch :: Git.Repository -> T.Text -> IO Git.Branch
findOrCreateBranch repo branch = do
    mbranch <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal
    maybe createBranch return mbranch
  where
    createBranch = do
        head_ref <- Git.repositoryGetHead repo >>= maybeThrow GetHeadError
        parent_obj <- Git.refLookup head_ref >>= maybeThrow RefLookupError
        Git.repositoryCreateBranch repo branch parent_obj [Git.CreateFlagsNone] >>= maybeThrow CreateBranchError

-- | Convert a Branch object to an OId
getBranchOIdFromObject :: Git.Repository -> Git.Branch -> IO Git.OId
getBranchOIdFromObject repo branch_obj = do
    branch_name <- Git.branchGetName branch_obj >>= maybeThrow BranchNameError
    let branch_ref = T.pack $ printf "refs/heads/%s" branch_name
    ref <- Git.repositoryLookupReference repo branch_ref >>= maybeThrow LookupReferenceError
    Git.refGetTarget ref >>= maybeThrow GetTargetError

-- | Make a new commit to a repository's branch
writeCommit :: Git.Repository -> T.Text -> T.Text -> T.Text -> BS.ByteString -> IO Git.OId
writeCommit repo branch filename message content = do
    parent_commit <- headCommit repo branch
    blob_id <- Git.repositoryCreateBlobFromBuffer repo content >>= maybeThrow CreateBlobError

    -- Use treebuilder to make a new entry for this filename and blob: repositoryCreateTreeBuilderFromTree
    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree >>= maybeThrow TreeBuilderError
    Git.treeBuilderInsert builder filename blob_id Git.FileModeBlob >>= maybeThrow BuilderInsertError
    (tree, sig, ref, encoding) <- prepareCommit repo branch builder
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit] >>= maybeThrow CreateCommitError

-- | Read a commit and return a ByteString of the content
-- TODO Return the commit message too
readCommit :: Git.Repository -> T.Text -> T.Text -> Maybe T.Text -> IO BS.ByteString
readCommit repo branch filename Nothing = do
    let spec = T.pack $ printf "%s:%s" branch filename
    readCommitSpec repo spec
readCommit repo _ filename commit = do
    let spec = T.pack $ printf "%s:%s" (fromJust commit) filename
    readCommitSpec repo spec

-- | Read a commit usinga revspec, return the ByteString content
readCommitSpec :: Git.Repository -> T.Text -> IO BS.ByteString
readCommitSpec repo spec = do
    obj <- Git.repositoryRevparse repo spec >>= maybeThrow RevparseError
    oid <- Git.objectGetId obj >>= maybeThrow GetIdError
    blob <- Git.repositoryLookupBlob repo oid >>= maybeThrow LookupBlobError
    Git.blobGetRawContent blob >>= maybeThrow GetRawBlobError

-- | Get the filename for a Blob tree entry
getFilename :: Git.Tree -> Word32 -> IO (Maybe T.Text)
getFilename tree idx = do
    entry <- Git.treeGet tree idx >>= maybeThrow GetTreeError

    -- Only allow Blob and BlobExecutable
    ifM (isFileBlob entry)
        (Just <$> Git.treeEntryGetName entry >>= maybeThrow GetNameError)
        (return Nothing)
 where
    isFileBlob entry = Git.treeEntryGetFileMode entry >>= \case
        Git.FileModeBlob           -> return True
        Git.FileModeBlobExecutable -> return True
        _                          -> return False

{-# ANN getFilenames ("HLint: ignore Eta reduce"::String) #-}
-- | Get a list of the Blob tree entry filenames
getFilenames :: Git.Tree -> Word32 -> IO [T.Text]
getFilenames tree idx = getFilenames' tree [] idx

-- | Build the list of filenames from the tree entries
getFilenames' :: Git.Tree -> [T.Text] -> Word32 -> IO [T.Text]
getFilenames' _ filenames 0 = return filenames
getFilenames' tree filenames idx = getFilename tree (idx-1) >>= \case
    Just name -> getFilenames' tree (name:filenames) (idx-1)
    Nothing   -> getFilenames' tree filenames (idx-1)

-- | List the files on a branch
listBranchFiles :: Git.Repository -> T.Text -> IO [T.Text]
listBranchFiles repo branch =
    headCommit repo branch >>= listCommitFiles repo

-- | List the files in a commit
listCommitFiles :: Git.Repository -> Git.Commit -> IO [T.Text]
listCommitFiles repo commit = do
    parent_tree_id <- Git.commitGetTreeId commit >>= maybeThrow GetTreeIdError
    tree <- Git.repositoryLookupTree repo parent_tree_id >>= maybeThrow LookupTreeError
    sz <- Git.treeSize tree
    getFilenames tree sz

deleteFile :: Git.Repository -> T.Text -> T.Text -> IO Git.OId
deleteFile repo branch filename = do
    parent_commit <- headCommit repo branch

    -- Use treebuilder to modify the tree
    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree >>= maybeThrow TreeBuilderError
    Git.treeBuilderRemove builder filename
    (tree, sig, ref, encoding) <- prepareCommit repo branch builder
    let message = T.pack $ printf "Recipe %s deleted" filename
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit] >>= maybeThrow  CreateCommitError

revertFile :: Git.Repository -> T.Text -> T.Text -> T.Text -> IO Git.OId
revertFile repo branch filename commit = do
    commit_id <- Git.oIdNewFromString commit >>= maybeThrow NewOIdError
    revertFileCommit repo branch filename commit_id

revertFileCommit :: Git.Repository -> T.Text -> T.Text -> Git.OId -> IO Git.OId
revertFileCommit repo branch filename commit_id = do
    commit_obj <- Git.repositoryLookupCommit repo commit_id >>= maybeThrow LookupCommitError
    revert_tree <- Git.commitGetTree commit_obj >>= maybeThrow GetTreeError
    entry <- Git.treeGetByName revert_tree filename >>= maybeThrow GetByNameError
    blob_id <- Git.treeEntryGetId entry >>= maybeThrow GetEntryIdError
    parent_commit <- headCommit repo branch

    -- Use treebuilder to modify the tree
    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree >>= maybeThrow TreeBuilderError
    Git.treeBuilderInsert builder filename blob_id Git.FileModeBlob
    (tree, sig, ref, encoding) <- prepareCommit repo branch builder
    commit <- Git.oIdToString commit_id >>= maybeThrow OIdError
    let message = T.pack $ printf "Recipe %s reverted to commit %s" filename commit
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit] >>= maybeThrow CreateCommitError

-- | File commit details
data CommitDetails =
    CommitDetails { cdCommit    :: T.Text
                  , cdTime      :: T.Text
                  , cdMessage   :: T.Text
                  , cdRevision  :: Maybe Int
    } deriving (Show, Eq)

listCommits :: Git.Repository -> T.Text -> T.Text -> IO [CommitDetails]
listCommits repo branch filename = do
    revwalk <- Git.revisionWalkerNew repo >>= maybeThrow NewWalkerError
    Git.revisionWalkerSetSortMode revwalk [Git.SortModeTime, Git.SortModeReverse]
    let branch_ref = T.pack $ printf "refs/heads/%s" branch
    Git.revisionWalkerPushRef revwalk branch_ref

    mfirst_id <- Git.revisionWalkerNext revwalk
    commitDetails repo revwalk branch filename [] mfirst_id

commitDetails :: Git.Repository -> Git.RevisionWalker -> T.Text -> T.Text -> [CommitDetails] -> Maybe Git.OId -> IO [CommitDetails]
commitDetails _ _ _ _ details Nothing = return details
commitDetails repo revwalk branch filename details next_id = do
    let commit_id = fromJust next_id
    commit_obj <- Git.repositoryLookupCommit repo commit_id >>= maybeThrow LookupCommitError

    parents <- Git.commitGetParents commit_obj >>= maybeThrow GetParentsError
    num_parents <- Git.commitParentsGetSize parents

    tree <- Git.commitGetTree commit_obj >>= maybeThrow GetTreeError

    is_diff <- if num_parents > 0
        then do
            commits <- mapM (getCommitParent parents) [0..num_parents-1]
            allM (parentDiff repo filename tree) commits
        else
            return False

    mnext_id <- Git.revisionWalkerNext revwalk
    mentry <- Git.treeGetByName tree filename
    if isJust mentry && is_diff
        then getCommitDetails commit_id commit_obj mnext_id
        else commitDetails repo revwalk branch filename details mnext_id

  where
    getCommitParent :: Git.CommitParents -> Word32 -> IO Git.Commit
    getCommitParent parents idx = Git.commitParentsGet parents idx >>= maybeThrow GetParentsError

    getCommitDetails :: Git.OId -> Git.Commit -> Maybe Git.OId -> IO [CommitDetails]
    getCommitDetails commit_id commit_obj mnext_id = do
        mtag <- findCommitTag repo branch filename commit_id
        let revision = getRevisionFromTag mtag
        -- Fill in a commit record
        message <- Git.commitGetMessage commit_obj >>= maybeThrow GetMessageError
        commit_str <- Git.oIdToString commit_id >>= maybeThrow OIdError
        sig <- Git.commitGetCommitter commit_obj >>= maybeThrow GetCommitterError

        -- XXX No Idea How To Convert These Yet
        datetime <- Git.signatureGetTime sig >>= maybeThrow GetTimeError
        timezone <- Git.signatureGetTimeZone sig >>= maybeThrow GetTimeZoneError
        -- What do you do with the TimeZone?
        timeval <- GLib.newZeroTimeVal
        ok <- GLib.dateTimeToTimeval datetime timeval
        -- XXX Handle error (ok == False)
        time_str <- GLib.timeValToIso8601 timeval

        let commit = CommitDetails {cdCommit=commit_str, cdTime=time_str, cdMessage=message, cdRevision=revision}
        commitDetails repo revwalk branch filename (commit:details) mnext_id

-- | Determine if there were changes between this commit and its parent
--
-- Return True if there were changes, False otherwise
parentDiff :: Git.Repository -> T.Text -> Git.Tree -> Git.Commit -> IO Bool
parentDiff repo filename commit_tree parent_commit = do
    diff_opts <- Git.diffOptionsNew >>= maybeThrow NewOptionsError
    Git.diffOptionsSetPathspec diff_opts (Just [filename])

    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    diff <- Git.diffNewTreeToTree repo (Just commit_tree) (Just parent_tree) (Just diff_opts) >>= maybeThrow NewTreeError
    num_deltas <- Git.diffGetNumDeltas diff
    return $ num_deltas > 0

-- | Find the revision tag pointing to a specific commit
--
-- Tag is of the form 'refs/tags/<branch>/<filename>/r<revision>
-- There should really only be one.
findCommitTag :: Git.Repository -> T.Text -> T.Text -> Git.OId -> IO (Maybe T.Text)
findCommitTag repo branch filename commit_id = do
    let tag_pattern = T.pack $ printf "%s/%s/r*" branch filename
    Git.repositoryListTagsMatch repo (Just tag_pattern) >>= \case
        Just []    -> return Nothing
        Just tags  -> filterTags tags
        Nothing    -> return Nothing
  where
    filterTags tags =
        maybeOneTag <$> filterM isCommitTag tags

    maybeOneTag :: [T.Text] -> Maybe T.Text
    maybeOneTag []    = Nothing
    maybeOneTag [tag] = Just tag
    maybeOneTag _     = Nothing

    -- | Return True if the tag is on the commit
    isCommitTag :: T.Text -> IO Bool
    isCommitTag tag = do
        -- Find the commit for this tag and check that it matches commit_id
        -- If so, return the branch/filename/r* part of the tag
        let ref_tag = T.pack $ printf "refs/tags/%s" tag
        ref <- Git.repositoryLookupReference repo ref_tag >>= maybeThrow LookupReferenceError
        tag_oid <- Git.refGetTarget ref >>= maybeThrow GetTargetError
        tag_obj <- Git.repositoryLookupTag repo tag_oid >>= maybeThrow LookupTagError
        oid <- Git.tagGetTargetId tag_obj >>= maybeThrow GetTargetIdError

        cmp <- Git.oIdCompare oid commit_id
        return $ cmp == 0

-- | Return the revision number from a git tag
-- Tag is of the form 'refs/tags/<branch>/<filename>/r<revision>
--
-- Returns a Just Int if all goes well, otherwise Nothing
getRevisionFromTag :: Maybe T.Text -> Maybe Int
getRevisionFromTag mtag = case mtag of
    Nothing  -> Nothing
    Just tag -> getRevision $ T.unpack tag
  where
    getRevision :: String -> Maybe Int
    getRevision tag = do
        -- Get the digits after the final r
        let rs = elemIndices 'r' tag
        if null rs
            then Nothing
            else readMaybe $ drop (last rs + 1) tag

-- | Tag a file's most recent commit
--
-- This uses git tags, of the form `refs/tags/<branch>/<filename>/r<revision>`
-- Only the most recent recipe commit can be tagged to prevent out of order tagging.
-- Revisions start at 1 and increment for each new commit that is tagged.
-- If the commit has already been tagged it will return false.
tagFileCommit :: Git.Repository -> T.Text -> T.Text -> IO Bool
tagFileCommit repo branch filename = do
    commits <- listCommits repo branch filename
    let rev_commit = findLastRev commits
    -- If there are no commits, or the most recent one has already been tagged, return False
    if null commits || isFirstCommit commits rev_commit
        then return False
        else tagNewestCommit (head commits) rev_commit
  where
    -- | Tag the most recent commit
    tagNewestCommit :: CommitDetails -> Maybe CommitDetails -> IO Bool
    tagNewestCommit last_commit rev_commit = do
        -- What revision is this? rev_commit may be Nothing, or cdRevision may be Nothing. Use 1 for those cases
        let rev = if isJust rev_commit && isJust (cdRevision (fromJust rev_commit))
                  then fromJust (cdRevision (fromJust rev_commit)) + 1
                  else 1

        let name = T.pack $ printf "%s/%s/r%d" branch filename rev
        sig <- Git.signatureNewNow "bdcs-cli" "user-email" >>= maybeThrow NewSignatureError
        commit_id <- Git.oIdNewFromString (cdCommit last_commit) >>= maybeThrow NewOIdError
        commit_type <- gobjectType (undefined :: Git.Commit)
        commit_obj <- Git.repositoryLookup repo commit_id commit_type >>= maybeThrow LookupError
        mtag_id <- Git.repositoryCreateTag repo name commit_obj sig name [Git.CreateFlagsNone]
        return $ isJust mtag_id

    -- | Find the last revision in the commits and return it
    findLastRev :: [CommitDetails] -> Maybe CommitDetails
    findLastRev []= Nothing
    findLastRev (x:xs) = case cdRevision x of
                             Nothing  -> findLastRev xs
                             Just _   -> Just x

    -- | Is the revision commit the most recent one?
    --
    -- If it is, then we cannot make a new tag.
    -- If it is not, or there is no rev_commit, we can tag a new one.
    isFirstCommit :: [CommitDetails] -> Maybe CommitDetails -> Bool
    isFirstCommit _ Nothing           = False
    isFirstCommit [] _                = False
    isFirstCommit (c:_) (Just commit) = commit == c


-- | Read and parse a recipe file
commitRecipeFile :: Git.Repository -> T.Text -> FilePath -> IO Git.OId
commitRecipeFile repo branch filename = do
    toml_in <- readFile filename
    let erecipe = parseRecipe (T.pack toml_in)
    -- XXX Handle errors
    let recipe = head $ rights [erecipe]
    commitRecipe repo branch recipe

-- | Commit a Recipe record to a branch
commitRecipe :: Git.Repository -> T.Text -> Recipe -> IO Git.OId
commitRecipe repo branch recipe = do
    let toml_out = encodeUtf8 $ recipeTOML recipe
    let filename = recipeTomlFilename (rName recipe)
    let eversion = recipeBumpVersion Nothing (rVersion recipe)
    -- XXX Handle errors
    let version = head $ rights [eversion]
    let message = T.pack $ printf "Recipe %s, version %s saved" filename version
    writeCommit repo branch filename message toml_out

-- | Commit recipes from a directory, if they don't already exist
--
commitRecipeDirectory :: Git.Repository -> T.Text -> FilePath -> IO [Git.OId]
commitRecipeDirectory repo branch directory = do
    branch_files <- listBranchFiles repo branch
    files <- map (directory ++) . filter (skipFiles branch_files) <$> listDirectory directory
    mapM (commitRecipeFile repo branch) files
  where
    skipFiles :: [T.Text] -> String -> Bool
    skipFiles branch_files file = T.pack file `notElem` branch_files && ".toml" `isSuffixOf` file

printOId :: Git.OId -> IO ()
printOId oid =
    Git.oIdToString oid >>= print




-- =========================
-- Test Functions Below Here
-- =========================
testRecipe :: Recipe
testRecipe =
    Recipe {rName = "test-server",
            rVersion = Just "0.1.2",
            rDescription = "Testing git commit of a Recipe record",
            rPackages = [RecipeModule {rmName = "tmux", rmVersion = "2.2"},
                         RecipeModule {rmName = "openssh-server", rmVersion = "6.6.*"},
                         RecipeModule {rmName = "rsync", rmVersion = "3.0.*"}],
            rModules = [RecipeModule {rmName = "httpd", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "mod_auth_kerb", rmVersion = "5.4"},
                        RecipeModule {rmName = "mod_ssl", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "php", rmVersion = "5.4.*"},
                        RecipeModule {rmName = "php-mysql", rmVersion = "5.4.*"}]
    }

testFiles :: [T.Text]
testFiles  = ["glusterfs.toml","http-server.toml","kubernetes.toml","test-server.toml"]
testFiles2 :: [T.Text]
testFiles2 = ["glusterfs.toml","kubernetes.toml","test-server.toml"]

data TestError =
    FileListError [T.Text]
  | ListCommitsError
  | HttpCommitError [CommitDetails]
  | TagCommitError
  | CommitRevisionError [CommitDetails]
  deriving (Eq, Show)

instance Exception TestError

runGitRepoTests :: IO Bool
runGitRepoTests = withTempDirectory "/var/tmp/" "bdcsgit-test" testGitRepo

testGitRepo :: FilePath -> IO Bool
testGitRepo tmpdir = do
    Git.init
    repo <- openOrCreateRepo tmpdir

    -- Commit a file to the repo
    putStrLn "    - Committing http-server.toml"
    commitRecipeFile repo "master" "./tests/recipes/http-server.toml"

    -- Commit a directory to the repo
    putStrLn "    - Committing a directory of recipes"
    commitRecipeDirectory repo "master" "./tests/recipes/"

    -- Commit a Recipe record to the repo
    putStrLn "    - Committing a Recipe record"
    commitRecipe repo "master" testRecipe

    -- List the files on master
    putStrLn "    - Listing the committed files"
    files <- listBranchFiles repo "master"
    unless (files == testFiles) (throwIO $ FileListError files)

    -- Get the commits to http-server.toml
    putStrLn "    - List commits to http-server.toml"
    http_commits <- listCommits repo "master" "http-server.toml"
    -- Should be 1 commit
    let expected_msg_1 = "Recipe http-server.toml, version 0.2.0 saved"
    let msg_1 = cdMessage (head http_commits)
    unless (msg_1 == expected_msg_1) (throwIO $ HttpCommitError http_commits)

    -- delete http-server.toml file
    putStrLn "    - Delete the http-server.toml file"
    deleteFile repo "master" "http-server.toml"

    -- List the files on master
    putStrLn "    - Check that http-server.toml has been deleted"
    files2 <- listBranchFiles repo "master"
    unless (files2 == testFiles2) (throwIO $ FileListError files2)

    -- Revert the delete
    commit_id <- Git.oIdNewFromString (cdCommit $ head http_commits) >>= maybeThrow NewOIdError
    revertFileCommit repo "master" "http-server.toml" commit_id

    -- List the files on master
    putStrLn "    - Check that http-server.toml has been restored"
    files3 <- listBranchFiles repo "master"
    unless (files3 == testFiles) (throwIO $ FileListError files3)

    -- tag a commit
    putStrLn "    - Tag most recent commit of http-server.toml"
    ok <- tagFileCommit repo "master" "http-server.toml"
    unless ok (throwIO TagCommitError)

    -- list the commits and check for the tag
    putStrLn "    - Check the Tag"
    commits <- listCommits repo "master" "http-server.toml"
    let revision = cdRevision (head commits)
    unless (revision == Just 1) (throwIO $ CommitRevisionError commits)

    return True
