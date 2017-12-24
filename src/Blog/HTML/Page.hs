{-| Page HTML
-}


{-# LANGUAGE OverloadedStrings #-}


module Blog.HTML.Page where


import Blog.Prelude
  ( ($), return, show
  , Text
  )
import Blog.Types.Page 
  ( Page (PageHome, PageArticle, PageAbout)
  , ArticlePage (ArticlePage)
  )
import Blog.Web.Config (cssFilePath, jsFilePath)

import Data.Monoid ((<>))
import Data.Foldable (forM_)

import System.FilePath (FilePath)

import Text.Blaze.Html5
  ( Html
  , (!), toValue, toHtml
  , preEscapedToHtml
  )

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H



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
      H.link ! A.href "https://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,600,700"
             ! A.rel "stylesheet"
      -- (2) Source Code Pro (monospace)
      H.link ! A.href "https://fonts.googleapis.com/css?family=Source+Code+Pro"
             ! A.rel "stylesheet"
    H.body $ do
      pageHeaderHtml pageType
      H.div ! A.id "content" $ contentHtml
      -- Scripts
      -- (1) Highlight.js
      H.script ! A.src (toValue $ jsFilePath "highlight") $ return ()
      -- (2) Other scripts
      forM_ jsFilePaths $ \path ->
        H.script ! A.src (toValue path) $ return ()
      -- (3) Initialization script
      H.script $ preEscapedToHtml pageScript


-- HEADER
--------------------------------------------------------------------------------

pageHeaderHtml :: Page -> Html
pageHeaderHtml page =
  H.header ! A.class_ "page-header" $ do
    H.div ! A.class_ "page-header-left" $ homeNavHtml page
    H.div ! A.class_ "page-header-center" $ pageNavHtml page
    H.div ! A.class_ "page-header-right" $ do
      aboutNavHtml page
      githubNavHtml
      donateNavHtml


-- Header > Left
--------------------------------------------------------------------------------

homeNavHtml :: Page -> Html
homeNavHtml page =
  H.a ! A.href "/"
      ! A.class_ classes
      -- $ preEscapedToHtml ("&#955; Haskell Blog Tutorial" :: Text)
      $ preEscapedToHtml ("Haskell Blog Tutorial" :: Text)
  where
    classes = case page of
                PageHome -> "home-nav selected"    
                _        -> "home-nav"    


-- Header > Center
--------------------------------------------------------------------------------

pageNavHtml :: Page -> Html
pageNavHtml (PageArticle articlePage) = articleNavHtml articlePage
pageNavHtml _                         = return ()


articleNavHtml :: ArticlePage -> Html
articleNavHtml (ArticlePage id title date) =
  H.a ! A.href (toValue $ "/articles" <> show id)
      ! A.class_ "article-nav selected" $ do
    H.span ! A.class_ "article-title" $ toHtml title
    H.span ! A.class_ "divider" $ "|"
    H.span ! A.class_ "article-date" $ toHtml date


-- Header > Right
--------------------------------------------------------------------------------
 
aboutNavHtml :: Page -> Html
aboutNavHtml page =
  H.a ! A.href "/about"
      ! A.class_ classes 
        $ "About Me"
  where
    classes = case page of
                PageAbout -> "about-nav selected"    
                _         -> "about-nav"    

githubNavHtml :: Html
githubNavHtml = H.a ! A.href linkHref $ "My Github"
  where
    linkHref = "www.github.com/jeff-wise/haskell-starter-kit-blog"


donateNavHtml :: Html
donateNavHtml = H.a ! A.href linkHref $ "Donate"
  where
    linkHref = "www.github.com/jeff-wise/haskell-starter-kit-blog"


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



pageScript :: Text
pageScript = "if (hljs) { hljs.initHighlightingOnLoad() };"

