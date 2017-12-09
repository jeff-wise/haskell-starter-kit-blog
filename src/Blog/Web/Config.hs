
{-| HTTP Configuration

-}


{-# LANGUAGE OverloadedStrings #-}


module Blog.Web.Config where


import Blog.Prelude (String)

import System.FilePath (FilePath, (</>), (<.>))



--------------------------------------------------------------------------------
-- PATHS
--------------------------------------------------------------------------------

cssFilePath :: String -> FilePath
cssFilePath cssName = "/static/css/" </> cssName <.> "css"
