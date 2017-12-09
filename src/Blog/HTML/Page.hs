{-| Page HTML
-}


{-# LANGUAGE OverloadedStrings #-}


module Blog.HTML.Page where


import Blog.Prelude
  ( ($), return
  , Text
  )
import Blog.Types.Article (ArticleTitle)
import Blog.Web.Config (cssFilePath)

import Data.Foldable (forM_)

import System.FilePath (FilePath)

import Text.Blaze.Html5
  ( Html
  , (!), toValue
  , preEscapedToHtml
  )

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H



--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

data Page =
    Home
  | Article ArticleTitle
  | About


--------------------------------------------------------------------------------
-- HTML
--------------------------------------------------------------------------------

pageHtml :: Page -> [FilePath] -> [FilePath ]-> Html -> Html
pageHtml pageType cssFilePaths jsFilePaths contentHtml = do
  H.docTypeHtml $ do
    H.head $ do
      -- Page Title
      H.title "My Haskell Blog"
      -- Default Stylesheets
      -- (1) Normalize
      H.link ! A.rel "stylesheet" 
             ! A.type_ "text/css" 
             ! A.href (toValue $ cssFilePath "normalize")
      -- (2) Page
      H.link ! A.rel "stylesheet" 
             ! A.type_ "text/css" 
             ! A.href (toValue $ cssFilePath "page")
      -- Other Stylesheets
      forM_ cssFilePaths $ \path ->
        H.link ! A.rel "stylesheet" 
               ! A.type_ "text/css" 
               ! A.href (toValue path)
      -- Fonts
      -- (1) Source Sans Pro (default)
      H.link ! A.href "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,600,700"
             ! A.rel "stylesheet"
      -- (2) Source Code Pro (monospace)
      H.link ! A.href "https://fonts.googleapis.com/css?family=Source+Code+Pro"
             ! A.rel "stylesheet"
    H.body $ do
      pageHeaderHtml pageType
      H.div ! A.id "content" $ contentHtml
      forM_ jsFilePaths $ \path ->
        H.script ! A.src (toValue path) $ return ()
      -- H.script $ preEscapedToHtml pageScript


pageHeaderHtml :: Page -> Html
pageHeaderHtml page = 
  H.header ! A.class_ "page-header" $ do
    H.a ! A.href "/"
        ! A.class_ homeLinkClass
        $ preEscapedToHtml ("&#955; My Haskell Blog" :: Text)
    H.h3 ! A.class_ "page-title" 
         ! A.class_ articleLinkClass
         $ ""
    H.div ! A.class_ "page-nav" $ do
      H.a ! A.href "/about"
          ! A.class_ aboutLinkClass 
          $ "About Me"
      H.a ! A.href "/" $ "My Github"
  where
    homeLinkClass = case page of 
                      Home -> "home selected"
                      _    -> "home"
    articleLinkClass = case page of 
                         Article _ -> "selected"
                         _    -> ""
    aboutLinkClass = case page of 
                       About -> "selected"
                       _    -> ""

                                


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
