
{-| Page HTML
-}


{-# LANGUAGE OverloadedStrings #-}


module Blog.HTML.Page where


import Blog.Common (($), return)
import Blog.Web.Config (cssFilePath)

import Data.Foldable (forM_)

import System.FilePath (FilePath)

import Text.Blaze.Html5
  ( Html
  , (!), toValue
  )

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H



page :: [FilePath] -> [FilePath ]-> Html -> Html
page cssFilePaths jsFilePaths contentHtml = do
  H.docTypeHtml $ do
    H.head $ do
      H.title "My Haskell Blog"
      H.link ! A.rel "stylesheet" 
             ! A.type_ "text/css" 
             ! A.href (toValue $ cssFilePath "page")
      forM_ cssFilePaths $ \path ->
        H.link ! A.rel "stylesheet" 
               ! A.type_ "text/css" 
               ! A.href (toValue path)
      H.link ! A.href "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700"
             ! A.rel "stylesheet"
      H.link ! A.href "https://fonts.googleapis.com/css?family=Source+Code+Pro"
             ! A.rel "stylesheet"
    H.body $ do
      pageHeaderHtml
      H.div ! A.id "content" $ contentHtml
      forM_ jsFilePaths $ \path ->
        H.script ! A.src (toValue path) $ return ()
      -- H.script $ preEscapedToHtml pageScript


pageHeaderHtml :: Html
pageHeaderHtml = H.div ! A.id "header" $ return ()

-- pageHeaderHtml :: SectionType -> Html -> Html
-- pageHeaderHtml sectionType subheaderHtml = do
--   H.div ! A.id "navbar" $ do
--     H.div ! A.id "navbar-left" $
--       H.a ! A.href "/" $
--         H.div ! A.class_ "home" $ do
--           H.div ! A.class_ "tome" $ "TOME"
--           H.div ! A.class_ "platform" $ "Tabletop RPG Platform"
--       --H.img ! (A.src $ toValue $ svgLink "tome")
--       --H.a ! A.href "/" $ "Tome"
--     H.div ! A.id "navbar-center" $ do
--       pageHeaderLinkHtml sectionType GettingStarted
--       pageHeaderLinkHtml sectionType Learn
--       pageHeaderLinkHtml sectionType Docs
--       pageHeaderLinkHtml sectionType News
--       pageHeaderLinkHtml sectionType Community
--     H.div ! A.id "navbar-right" $ return ()
--   subheaderHtml


-- -- pageHeaderLinkHtml :: Text -> Bool -> Html
-- -- pageHeaderLinkHtml linkText isSelected = 
-- --   if isSelected
-- --      then H.a ! A.class_ "selected" $ toHtml linkText
-- --      else H.a $ toHtml linkText


-- pageHeaderLinkHtml :: SectionType -> SectionType -> Html
-- pageHeaderLinkHtml sectionType linkType = 
--   H.a ! A.class_ classes
--       ! A.href (toValue $ pageLink linkType) 
--       $ toHtml $ show linkType
--   where
--     classes = if sectionType == linkType
--                  then "selected" 
--                  else ""


-- pageFooterHtml :: Html
-- pageFooterHtml = H.div ! A.id "footer" $ return ()



-- pageScript :: String
-- pageScript = 
--      "document.addEventListener('DOMContentLoaded', function(event) { "
--   <> "    if (hljs != null) { hljs.initHighlightingOnLoad() };"
--   <> "});"
