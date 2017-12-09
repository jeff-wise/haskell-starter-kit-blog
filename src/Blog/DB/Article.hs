
{-| Article DB Types
   
-}


{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Blog.DB.Article where


import Blog.Prelude
  ( Int, Maybe (Nothing, Just), Text
  , IO
  , ($), (<$>)
  , return
  )
import Blog.Types.Article 
  ( Article (Article), articleId, articleTitle
  , articleTimeCreated, articleSummary, articleBody
  , ArticleId (ArticleId), getArticleId
  , ArticleTitle (ArticleTitle), getArticleTitle
  , ArticleTimeCreated (ArticleTimeCreated), getArticleTimeCreated
  , ArticleSummary (ArticleSummary), getArticleSummary
  , ArticleBody (ArticleBody), getArticleBody 
  , NewArticle, newArticleTitle, newArticleTimeCreated
  , newArticleSummary, newArticleBody
  )

import Control.Arrow (returnA)

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime, getCurrentTime)

import Opaleye
  ( Table (Table), Column
  , PGInt4, PGText, PGTimestamptz
  , Query, queryTable
  , required, optional
  , pgInt4, pgStrictText, pgUTCTime
  , restrict
  , (.==)
  )



data DB_Article' a b c d e = DB_Article 
  { dbArticleId          :: a
  , dbArticleTitle       :: b
  , dbArticleCreatedTime :: c 
  , dbArticleSummary     :: d
  , dbArticleBody        :: e
  }


type DB_Article = DB_Article' Int Text UTCTime Text Text


type ArticleRowRead = DB_Article' (Column PGInt4) 
                                  (Column PGText) 
                                  (Column PGTimestamptz) 
                                  (Column PGText)
                                  (Column PGText)
type ArticleRowWrite = DB_Article' (Maybe (Column PGInt4))
                                   (Column PGText) 
                                   (Column PGTimestamptz) 
                                   (Column PGText)
                                   (Column PGText)


$(makeAdaptorAndInstance "pArticle" ''DB_Article')


articleTable :: Table ArticleRowWrite ArticleRowRead 
articleTable = Table "article"
                    (pArticle $ DB_Article (optional "id"          )
                                           (required "title"       ) 
                                           (required "time_created")
                                           (required "summary"     )
                                           (required "body"        ))


articleNewRow :: NewArticle -> IO ArticleRowWrite
articleNewRow article = do
  dbTime <- case newArticleTimeCreated article of
              Just timeCreated -> return $ pgUTCTime $ getArticleTimeCreated timeCreated
              Nothing          -> pgUTCTime <$> getCurrentTime
  return $ DB_Article Nothing
                      (pgStrictText $ getArticleTitle $ newArticleTitle article)
                      dbTime
                      (pgStrictText $ getArticleSummary $ newArticleSummary article)
                      (pgStrictText $ getArticleBody $ newArticleBody article)


articleUpdateRow :: Article -> ArticleRowWrite
articleUpdateRow article =
  DB_Article (Just $ pgInt4 $ getArticleId $ articleId article)
             (pgStrictText $ getArticleTitle $ articleTitle article)
             (pgUTCTime $ getArticleTimeCreated $ articleTimeCreated article)
             (pgStrictText $ getArticleSummary $ articleSummary article)
             (pgStrictText $ getArticleBody $ articleBody article)


allArticlesQuery :: Query ArticleRowRead
allArticlesQuery = queryTable articleTable


articleWithIdQuery :: Int -> Query ArticleRowRead
articleWithIdQuery targetId = proc () -> do
  article@(DB_Article rowId _ _ _ _) <- allArticlesQuery -< ()
  restrict -< rowId .== pgInt4 targetId
  returnA -< article


articleFromDB :: DB_Article -> Article
articleFromDB dbArticle = Article 
  (ArticleId $ dbArticleId dbArticle)
  (ArticleTitle $ dbArticleTitle dbArticle)
  (ArticleTimeCreated $ dbArticleCreatedTime dbArticle)
  (ArticleSummary $ dbArticleSummary dbArticle)
  (ArticleBody $ dbArticleBody dbArticle)

