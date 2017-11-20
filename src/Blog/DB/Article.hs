
{-| Article DB Types
   
-}


{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Blog.DB.Article where


import Blog.Common
  ( Int, Maybe (Nothing, Just), Text
  , IO
  , ($), (.)
  , return
  )
import Blog.Types.Article 
  ( Article (Article), articleId, articleTitle, articleCreatedTime, articleBody
  , ArticleId (ArticleId), getArticleId
  , ArticleTitle (ArticleTitle), getArticleTitle
  , ArticleCreatedTime (ArticleCreatedTime), getArticleCreatedTime
  , ArticleBody (ArticleBody), getArticleBody 
  , NewArticle, newArticleTitle, newArticleBody
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



data DB_Article' a b c d = DB_Article 
  { dbArticleId          :: a
  , dbArticleTitle       :: b
  , dbArticleCreatedTime :: c 
  , dbArticleBody        :: d
  }


type DB_Article = DB_Article' Int Text UTCTime Text


type ArticleRowRead = DB_Article' (Column PGInt4) 
                                  (Column PGText) 
                                  (Column PGTimestamptz) 
                                  (Column PGText)
type ArticleRowWrite = DB_Article' (Maybe (Column PGInt4))
                                   (Column PGText) 
                                   (Column PGTimestamptz) 
                                   (Column PGText)


$(makeAdaptorAndInstance "pArticle" ''DB_Article')


articleTable :: Table ArticleRowWrite ArticleRowRead 
articleTable = Table "article"
                    (pArticle $ DB_Article (optional "id"   )
                                           (required "title") 
                                           (required "date" )
                                           (required "body" ))


articleNewRow :: NewArticle -> IO ArticleRowWrite
articleNewRow article = do
  currentTime <- getCurrentTime
  return $ DB_Article Nothing
                     (pgStrictText $ getArticleTitle $ newArticleTitle article)
                     (pgUTCTime currentTime)
                     (pgStrictText $ getArticleBody $ newArticleBody article)


articleUpdateRow :: Article -> ArticleRowWrite
articleUpdateRow article =
  DB_Article (Just $ pgInt4 $ getArticleId $ articleId article)
             (pgStrictText $ getArticleTitle $ articleTitle article)
             (pgUTCTime $ getArticleCreatedTime $ articleCreatedTime article)
             (pgStrictText $ getArticleBody $ articleBody article)


allArticlesQuery :: Query ArticleRowRead
allArticlesQuery = queryTable articleTable


articleWithIdQuery :: Int -> Query ArticleRowRead
articleWithIdQuery targetId = proc () -> do
  article@(DB_Article rowId _ _ _) <- allArticlesQuery -< ()
  restrict -< rowId .== pgInt4 targetId
  returnA -< article


articleFromDB :: DB_Article -> Article
articleFromDB dbArticle = Article 
  (ArticleId $ dbArticleId dbArticle)
  (ArticleTitle $ dbArticleTitle dbArticle)
  (ArticleCreatedTime $ dbArticleCreatedTime dbArticle)
  (ArticleBody $ dbArticleBody dbArticle)

