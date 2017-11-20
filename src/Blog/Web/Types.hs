
{-| HTTP Types
-}


{-# LANGUAGE OverloadedStrings #-}


module Blog.Web.Types where


import Blog.Types (Env)

import Control.Monad.Reader (ReaderT)

import Servant (Handler)



-- HANDLER MONAD
--------------------------------------------------------------------------------

type SiteHandler = ReaderT Env Handler


-- data User = User


-- -- RESOURCE PATHS
-- --------------------------------------------------------------------------------

-- documentCssPath :: String
-- documentCssPath = "/static/css/page.css"

-- tutorialIndexCssPath :: Text
-- tutorialIndexCssPath = "/static/css/tutorial_index.css"

-- gsAmanaceAndroidCssPath :: Text
-- gsAmanaceAndroidCssPath = "/static/css/gs-amanace-android.css"

-- tutorialCssPath :: Text
-- tutorialCssPath = "/static/css/tutorial.css"

-- newsCssPath :: Text
-- newsCssPath = "/static/css/news.css"

-- docsCssPath :: Text
-- docsCssPath = "/static/css/docs.css"

-- blogCssPath :: Text
-- blogCssPath = "/static/css/blog.css"

-- yamlCssPath :: Text
-- yamlCssPath = "/static/css/yaml.css"

-- jsPath :: Text -> Text
-- jsPath scriptName = "/static/js/" <> scriptName <> ".js"

-- svgLink :: Text -> Text
-- svgLink svgName = "/static/svg/" <> svgName <> ".svg"

-- imageLink :: Text -> Text
-- imageLink imageName = "/images/" <> imageName

-- cssLink :: Text -> Text
-- cssLink styleSheetName = "/static/css/" <> styleSheetName <> ".css"

-- videoLink :: Text -> Text
-- videoLink videoName = "/static/videos/" <> videoName <> ".mp4"

