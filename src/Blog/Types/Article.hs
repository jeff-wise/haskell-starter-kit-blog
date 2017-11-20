
{-| Article
   
  This module defines the Article type and associated instances.
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Blog.Types.Article where


import Blog.Common
  ( Eq, Int, Text
  , ($), (<$>), (<*>), (<>)
  , return
  )

import Data.Aeson
  ( FromJSON (parseJSON), ToJSON (toJSON, toEncoding)
  , (.:), (.=)
  , withObject, object, pairs
  )
import Data.Time (UTCTime)

import Text.Blaze.Html5 (ToMarkup (toMarkup))



-- ARTICLE
--------------------------------------------------------------------------------

data Article = Article
  { articleId          :: ArticleId
  , articleTitle       :: ArticleTitle
  , articleCreatedTime :: ArticleCreatedTime
  , articleBody        :: ArticleBody
  } deriving (Eq)


-- * JSON
--------------------------------------------------------------------------------

instance FromJSON Article where
  parseJSON = withObject "Article" $ \obj -> Article
    <$> obj .: "id"
    <*> obj .: "title"
    <*> obj .: "created_time"
    <*> obj .: "body"


instance ToJSON Article where
  toJSON (Article id title createdTime body) = object 
    [ "id" .= id
    , "title" .= title
    , "created_time" .= createdTime
    , "body" .= body
    ]
  toEncoding (Article id title createdTime body) = pairs (
        "id" .= id
    <>  "title" .= title
    <>  "created_time" .= createdTime
    <>  "body" .= body
    )


-- * HTML
--------------------------------------------------------------------------------

instance ToMarkup Article where
  toMarkup _ = return ()


-- Article > Id
--------------------------------------------------------------------------------

newtype ArticleId = ArticleId
  { getArticleId :: Int }
  deriving (Eq, FromJSON, ToJSON)


-- Article > Title
--------------------------------------------------------------------------------

newtype ArticleTitle = ArticleTitle
  { getArticleTitle :: Text }
  deriving (Eq, FromJSON, ToJSON)


-- Article > Created Time
--------------------------------------------------------------------------------

newtype ArticleCreatedTime = ArticleCreatedTime
  { getArticleCreatedTime :: UTCTime }
  deriving (Eq, FromJSON, ToJSON)


-- Article > Body
--------------------------------------------------------------------------------

newtype ArticleBody = ArticleBody
  { getArticleBody :: Text }
  deriving (Eq, FromJSON, ToJSON)


-- NEW ARTICLE
--------------------------------------------------------------------------------

data NewArticle = NewArticle
  { newArticleTitle :: ArticleTitle
  , newArticleBody  :: ArticleBody
  } deriving (Eq)


-- * JSON
--------------------------------------------------------------------------------
instance FromJSON NewArticle where
  parseJSON = withObject "NewArticle" $ \obj -> NewArticle
    <$> obj .: "title"
    <*> obj .: "body"


instance ToJSON NewArticle where
  toJSON (NewArticle title body) = object 
    [ "title" .= title
    , "body" .= body
    ]
  toEncoding (NewArticle title body) = pairs (
        "title" .= title
    <>  "body" .= body
    )


