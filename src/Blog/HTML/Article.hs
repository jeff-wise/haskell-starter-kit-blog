
{-| Article HTML
-}


{-# LANGUAGE OverloadedStrings #-}


module Blog.HTML.Article
  ( articlePageHtml
  ) where


import Blog.Prelude (($), Text)

import Blog.HTML.Page
  ( pageHtml
  ) 
import Blog.Types.Article 
  ( Article (Article)
  , ArticleTimeCreated (ArticleTimeCreated)
  , getArticleId, getArticleTitle, getArticleBody
  )
import Blog.Types.Page 
  ( Page (PageArticle)
  , ArticlePage (ArticlePage)
  )
import Blog.Web.Config (cssFilePath)

import qualified Data.Text as T (pack)
import qualified Data.Text.Lazy as LT (fromStrict)
import Data.Time.Format 
  ( formatTime
  , defaultTimeLocale
  )

import Text.Blaze.Html5
  ( Html
  , (!), toHtml
  , ToMarkup, toMarkup
  )

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

import Text.Markdown (markdown, defaultMarkdownSettings)



--------------------------------------------------------------------------------
-- INSTANCE
--------------------------------------------------------------------------------

instance ToMarkup Article where
  toMarkup = articlePageHtml


-------------------------------------------------------------------------------
-- PAGE HTML
--------------------------------------------------------------------------------

articlePageHtml :: Article -> Html
articlePageHtml article@(Article id title timeCreated _ _) = do
  let articlePage = PageArticle $ ArticlePage 
                      (getArticleId id)
                      (getArticleTitle title)
                      (dateText timeCreated)
  pageHtml articlePage
           [cssFilePath "article"] 
           []
           (articlePageContentHtml article)


articlePageContentHtml :: Article -> Html
articlePageContentHtml = articleHtml


articleHtml :: Article -> Html
articleHtml (Article _ title timeCreated _ body) =
  H.div ! A.class_ "article-container" $ do
    H.div ! A.class_ "article" $ do
      titleHtml
      dateHtml
      bodyHtml
  where
    -- Title
    titleHtml = H.h1 $ toHtml $ getArticleTitle title
    -- Date
    dateHtml = H.div ! A.class_ "article-date" $ do
      H.span ! A.class_ "published" $ "published on"
      H.span ! A.class_ "date" $ toHtml $ dateText timeCreated
    -- Body
    bodyHtml = 
      H.div ! A.class_ "article-body" $ do
        let bodyText = LT.fromStrict $ getArticleBody body
        toHtml $ markdown defaultMarkdownSettings bodyText
   

dateText :: ArticleTimeCreated -> Text
dateText (ArticleTimeCreated utcTime) =
  T.pack $ formatTime defaultTimeLocale "%b %d, %Y" utcTime
