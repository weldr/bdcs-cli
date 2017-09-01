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
                      commitRecipeDirectory)
  where

import           Control.Conditional(ifM)
import           Control.Exception
import           Control.Monad(filterM)
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

-- | Open a Git repository, or create the initial repository if one doesn't exist
openOrCreateRepo :: FilePath -> IO Git.Repository
openOrCreateRepo path = do
    gfile <- fileNewForPath path
    ifM (doesPathExist path)
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
    -- does the branch exist? If so get its OId: repositoryLookupBranch
    -- If it does not, create it and get its OId: repositoryCreateBranch
    branch_obj <- findOrCreateBranch repo branch
    branch_id <- getBranchOIdFromObject repo branch_obj
    parent_commit <- Git.repositoryLookupCommit repo branch_id >>= maybeThrow LookupCommitError
    blob_id <- Git.repositoryCreateBlobFromBuffer repo content >>= maybeThrow CreateBlobError

    -- Use treebuilder to make a new entry for this filename and blob: repositoryCreateTreeBuilderFromTree
    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree >>= maybeThrow TreeBuilderError
    Git.treeBuilderInsert builder filename blob_id Git.FileModeBlob >>= maybeThrow BuilderInsertError
    tree_id <- Git.treeBuilderWrite builder >>= maybeThrow BuilderWriteError
    tree <- Git.repositoryLookupTree repo tree_id >>= maybeThrow LookupTreeError

    -- Create a signature
    sig <- Git.signatureNewNow "bdcs-cli" "user-email" >>= maybeThrow NewSignatureError

    let ref = Just $ T.pack $ printf "refs/heads/%s" branch
    let encoding = Just "UTF-8"

    -- Create a new commit: repositoryCreateCommit
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
    isBlob <- isFileBlob entry
    if isBlob
        then do
            name <- Git.treeEntryGetName entry >>= maybeThrow GetNameError
            return $ Just name
        else return Nothing
 where
    isFileBlob entry = do
        mode <- Git.treeEntryGetFileMode entry
        case mode of
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
getFilenames' tree filenames idx = do
    filename <- getFilename tree (idx-1)
    case filename of
        Just name -> getFilenames' tree (name:filenames) (idx-1)
        Nothing   -> getFilenames' tree filenames (idx-1)

-- | List the files on a branch
listBranchFiles :: Git.Repository -> T.Text -> IO [T.Text]
listBranchFiles repo branch = do
    branch_obj <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal >>= maybeThrow LookupBranchError
    branch_id <- getBranchOIdFromObject repo branch_obj
    -- get the parent commit for this branch: repositoryLookupCommit
    parent_commit <- Git.repositoryLookupCommit repo branch_id >>= maybeThrow LookupCommitError
    listCommitFiles repo parent_commit

-- | List the files in a commit
listCommitFiles :: Git.Repository -> Git.Commit -> IO [T.Text]
listCommitFiles repo commit = do
    parent_tree_id <- Git.commitGetTreeId commit >>= maybeThrow GetTreeIdError
    tree <- Git.repositoryLookupTree repo parent_tree_id >>= maybeThrow LookupTreeError
    sz <- Git.treeSize tree
    getFilenames tree sz

deleteFile :: Git.Repository -> T.Text -> T.Text -> IO Git.OId
deleteFile repo branch filename = do
    branch_obj <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal >>= maybeThrow LookupBranchError
    branch_id <- getBranchOIdFromObject repo branch_obj
    parent_commit <- Git.repositoryLookupCommit repo branch_id >>= maybeThrow LookupCommitError

    -- Use treebuilder to modify the tree
    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree >>= maybeThrow TreeBuilderError
    Git.treeBuilderRemove builder filename
    tree_id <- Git.treeBuilderWrite builder >>= maybeThrow BuilderWriteError
    tree <- Git.repositoryLookupTree repo tree_id >>= maybeThrow LookupTreeError

    -- Create a signature
    sig <- Git.signatureNewNow "bdcs-cli" "user-email" >>= maybeThrow NewSignatureError

    let ref = Just $ T.pack $ printf "refs/heads/%s" branch
    let encoding = Just "UTF-8"

    let message = T.pack $ printf "Recipe %s deleted" filename

    -- Create a new commit: repositoryCreateCommit
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

-- vvv This could be a function, it's used in multiple places
    branch_obj <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal >>= maybeThrow LookupBranchError
    branch_id <- getBranchOIdFromObject repo branch_obj
    parent_commit <- Git.repositoryLookupCommit repo branch_id >>= maybeThrow LookupCommitError
-- ^^^ This could be a function, it's used in multiple places

    -- Use treebuilder to modify the tree
    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree >>= maybeThrow TreeBuilderError

    Git.treeBuilderInsert builder filename blob_id Git.FileModeBlob
    tree_id <- Git.treeBuilderWrite builder >>= maybeThrow BuilderWriteError
    tree <- Git.repositoryLookupTree repo tree_id >>= maybeThrow LookupTreeError

    -- Create a signature
    sig <- Git.signatureNewNow "bdcs-cli" "user-email" >>= maybeThrow NewSignatureError

    let ref = Just $ T.pack $ printf "refs/heads/%s" branch
    let encoding = Just "UTF-8"

    commit <- Git.oIdToString commit_id >>= maybeThrow OIdError
    let message = T.pack $ printf "Recipe %s reverted to commit %s" filename commit

    -- Create a new commit: repositoryCreateCommit
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
    if num_deltas > 0
    then return True
    else return False

-- | Find the revision tag pointing to a specific commit
--
-- Tag is of the form 'refs/tags/<branch>/<filename>/r<revision>
-- There should really only be one.
findCommitTag :: Git.Repository -> T.Text -> T.Text -> Git.OId -> IO (Maybe T.Text)
findCommitTag repo branch filename commit_id = do
    let tag_pattern = T.pack $ printf "%s/%s/r*" branch filename
    mall_tags <- Git.repositoryListTagsMatch repo (Just tag_pattern)
    case mall_tags of
        Just []    -> return Nothing
        Just tags  -> filterTags tags
        Nothing    -> return Nothing
  where
    filterTags tags = do
        commit_tags <- filterM isCommitTag tags
        return $ maybeOneTag commit_tags

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
        if cmp == 0
            then return True
            else return False

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
        if isJust mtag_id
            then return True
            else return False

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
    isFirstCommit commits rev_commit = case rev_commit of
                                           Nothing -> False
                                           Just commit -> commit == head commits


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
printOId oid = do
    moid_str <- Git.oIdToString oid
    print moid_str

-- | Test out git functions
doGitTests :: FilePath -> IO ()
doGitTests path = do
    -- Move this elsewhere later, MUST be called first
    Git.init
    repo <- openOrCreateRepo path
-- WORKS
    commit_id <- writeCommit repo "master" "README" "A test commit\n\nWith some commentary." "Some Content"
    commit_id <- writeCommit repo "master" "README" "A test commit 2\n\nWith some other commentary." "Some Content #2"
    commit_id <- writeCommit repo "master" "TODO" "A todo commit\n\nWith some stuff to do." "Some Content"
    commit_id <- writeCommit repo "master" "TODO" "A todo commit\n\nWith some commentary." "Some other Content"

    files <- listBranchFiles repo "master"
    print files

    commit_id <- writeCommit repo "master" "FRODO" "A list of stuff to be accomplished\n\nFind a magic ring" "Some other Content"
    files <- listBranchFiles repo "master"
    print files

    deleteFile repo "master" "FRODO"

    files <- listBranchFiles repo "master"
    print files

    -- Revert the delete
    revertFileCommit repo "master" "FRODO" commit_id

    files <- listBranchFiles repo "master"
    print files

    commits <- listCommits repo "master" "FRODO"
    print commits

    commits <- listCommits repo "master" "TODO"
    print commits

    -- First is True, 2nd is False (because there are no new commits)
    ok <- tagFileCommit repo "master" "FRODO"
    print ok
    ok <- tagFileCommit repo "master" "FRODO"
    print ok

    commitRecipeDirectory repo "master" "/home/bcl/tmp/recipes/"

    return ()
