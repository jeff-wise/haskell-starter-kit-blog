
{-| Article
   
  This module defines the Article type and associated instances.
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Blog.Types.Article where


import Blog.Prelude
  ( Int, Text, Maybe
  , Eq, Ord, Show (show)
  , ($), (<$>), (<*>), (<>)
  )

import Data.Aeson
  ( FromJSON (parseJSON), ToJSON (toJSON, toEncoding)
  , (.:), (.:?), (.=), (.!=)
  , withObject, object, pairs
  )
import Data.Time (UTCTime)



-- ARTICLE
--------------------------------------------------------------------------------

data Article = Article
  { articleId          :: ArticleId
  , articleTitle       :: ArticleTitle
  , articleTimeCreated :: ArticleTimeCreated
  , articleSummary     :: ArticleSummary
  , articleBody        :: ArticleBody
  } deriving (Eq)


-- * JSON
--------------------------------------------------------------------------------

instance FromJSON Article where
  parseJSON = withObject "Article" $ \obj -> Article
    <$> obj .: "id"
    <*> obj .: "title"
    <*> obj .: "time_created"
    <*> obj .:? "summary" .!= (ArticleSummary "")
    <*> obj .: "body"


instance ToJSON Article where
  toJSON (Article id title timeCreated summary body) = object 
    [ "id" .= id
    , "title" .= title
    , "time_created" .= timeCreated
    , "summary" .= summary
    , "body" .= body
    ]
  toEncoding (Article id title timeCreated summary body) = pairs (
       "id" .= id
    <> "title" .= title
    <> "time_created" .= timeCreated
    <> "summary" .= summary
    <> "body" .= body
    )


-- Article > Id
--------------------------------------------------------------------------------

newtype ArticleId = ArticleId
  { getArticleId :: Int }
  deriving (Eq, FromJSON, ToJSON)


instance Show ArticleId where
  show (ArticleId id) = show id


-- Article > Title
--------------------------------------------------------------------------------

newtype ArticleTitle = ArticleTitle
  { getArticleTitle :: Text }
  deriving (Eq, FromJSON, ToJSON)


-- Article > Created Time
--------------------------------------------------------------------------------

newtype ArticleTimeCreated = ArticleTimeCreated
  { getArticleTimeCreated :: UTCTime }
  deriving (Eq, Ord, FromJSON, ToJSON)


-- Article > Summary
--------------------------------------------------------------------------------

newtype ArticleSummary = ArticleSummary
  { getArticleSummary :: Text }
  deriving (Eq, FromJSON, ToJSON)


-- Article > Body
--------------------------------------------------------------------------------

newtype ArticleBody = ArticleBody
  { getArticleBody :: Text }
  deriving (Eq, FromJSON, ToJSON)


-- NEW ARTICLE
--------------------------------------------------------------------------------

data NewArticle = NewArticle
  { newArticleTitle       :: ArticleTitle
  , newArticleTimeCreated :: Maybe ArticleTimeCreated
  , newArticleBody        :: ArticleBody
  , newArticleSummary     :: ArticleSummary
  } deriving (Eq)


-- * JSON
--------------------------------------------------------------------------------
instance FromJSON NewArticle where
  parseJSON = withObject "NewArticle" $ \obj -> NewArticle
    <$> obj .:  "title"
    <*> obj .:? "time_created"
    <*> obj .:  "body"
    <*> obj .:? "summary" .!= (ArticleSummary "")


instance ToJSON NewArticle where
  toJSON (NewArticle title timeCreated body summary) = object 
    [ "title"        .= title
    , "body"         .= body
    , "time_created" .= timeCreated
    , "summary"      .= summary
    ]
  toEncoding (NewArticle title timeCreated body summary) = pairs (
       "title"        .= title
    <> "body"         .= body
    <> "time_created" .= timeCreated
    <> "summary"      .= summary
    )


-- ARTICLE UPDATE
--------------------------------------------------------------------------------

data ArticleUpdate = ArticleUpdate
  { articleUpdateTitle   :: Maybe ArticleTitle
  , articleUpdateSummary :: Maybe ArticleSummary
  , articleUpdateBody    :: Maybe ArticleBody
  } deriving (Eq)


-- * JSON
--------------------------------------------------------------------------------

instance FromJSON ArticleUpdate where
  parseJSON = withObject "ArticleUpdate" $ \obj -> ArticleUpdate
    <$> obj .:? "title"
    <*> obj .:? "summary"
    <*> obj .:? "body"

