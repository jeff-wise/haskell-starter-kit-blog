
{-| Site Paths

   Filesystem paths for the server.
-}


{-# LANGUAGE OverloadedStrings #-}


module Blog.Config where


import System.FilePath (FilePath, (</>))



--------------------------------------------------------------------------------
-- FILE PATHS
--------------------------------------------------------------------------------

siteDirPath :: FilePath
siteDirPath = "/site"


staticDirPath :: FilePath
staticDirPath = siteDirPath </> "static/"

