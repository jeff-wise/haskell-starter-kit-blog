
{-| ArticleList HTML
-}


{-# LANGUAGE OverloadedStrings #-}


module Blog.HTML.ArticleList 
  ( articleListPageHtml
  ) where


import Blog.Prelude (($), Text, String, show, return)

import Blog.HTML.Page
  ( pageHtml
  ) 

import Blog.Types.Article 
  ( Article (Article)
  , articleTimeCreated
  , getArticleTitle, getArticleTimeCreated, getArticleSummary
  )
import Blog.Types.Page (Page (PageHome))
import Blog.Web.Config (cssFilePath)

import Data.Foldable (forM_)
import Data.List (reverse, sortOn)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as LT (fromStrict)
import Data.Time.Format 
  ( formatTime
  , defaultTimeLocale
  )

import Text.Blaze.Html5
  ( Html
  , (!), toHtml, toValue
  , preEscapedToHtml
  )

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

import Text.Markdown (markdown, defaultMarkdownSettings)



articleListPageHtml :: [Article] -> Html
articleListPageHtml articles = 
  pageHtml PageHome [cssFilePath "home"] [] $ articleListHtml articles


articleListHtml :: [Article] -> Html
articleListHtml articles = do
  let sortedArticles = reverse $ sortOn articleTimeCreated articles
  H.div ! A.class_ "article-list" $ _html sortedArticles
  where
    _html []                           = return ()
    _html (latestArticle:pastArticles) = do
      latestArticleSummaryHtml latestArticle
      pastArticleListHtml pastArticles
      

latestArticleSummaryHtml :: Article -> Html
latestArticleSummaryHtml (Article _ title timeCreated summary _) =
  H.div ! A.class_ "latest-article-summary-container" $ do
    H.div ! A.class_ "latest-article-summary" $ do
      titleHtml
      dateHtml
      summaryHtml
      readButtonHtml
  where
    titleHtml = H.h1 $ toHtml $ getArticleTitle title
    dateHtml = 
      H.div ! A.class_ "latest-article-summary-date" $ do
        let timeString = formatTime defaultTimeLocale timeFormatString $ 
                           getArticleTimeCreated timeCreated
        toHtml timeString 
    summaryHtml = 
      H.div ! A.class_ "latest-article-summary-text" $ do
        let summaryText = LT.fromStrict $ getArticleSummary summary
        toHtml $ markdown defaultMarkdownSettings summaryText
    readButtonHtml = 
      H.button ! A.class_ "read-more" $ do
        H.span "READ ARTICLE"
        H.span ! A.class_ "arrow" $ preEscapedToHtml ("&#8594;" :: Text)
        


pastArticleListHtml :: [Article] -> Html
pastArticleListHtml articles =
  H.div ! A.class_ "past-articles-container" $
    H.div ! A.class_ "past-articles" $ do
      H.h2 "Past Articles"
      H.div ! A.class_ "article-list" $ do
        forM_ articles articleSummaryHtml
  where
    articleSummaryHtml :: Article -> Html
    articleSummaryHtml (Article id title timeCreated _ _) =
      H.a ! A.class_ "article-summary" 
          ! A.href (toValue $ "/articles/" <> show id) $ do
        H.div ! A.class_ "article-summary-title" $ 
          toHtml $ getArticleTitle title
        H.div ! A.class_ "article-summary-date" $ do
          let timeString = formatTime defaultTimeLocale timeFormatString $ 
                             getArticleTimeCreated timeCreated
          toHtml timeString


timeFormatString :: String
timeFormatString = "%b %d, %Y"
