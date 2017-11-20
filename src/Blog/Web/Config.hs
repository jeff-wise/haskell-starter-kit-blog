
{-| HTTP Configuration

-}


{-# LANGUAGE OverloadedStrings #-}


module Blog.Web.Config where


import Blog.Common (String)

import System.FilePath (FilePath, (</>), (<.>))



--------------------------------------------------------------------------------
-- PATHS
--------------------------------------------------------------------------------

cssFilePath :: String -> FilePath
cssFilePath cssName = "/static/css/" </> cssName <.> "css"
