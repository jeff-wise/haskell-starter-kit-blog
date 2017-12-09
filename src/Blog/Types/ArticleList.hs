
{-| Article List
   
  This module defines the Article List type and its typeclass instances.

  An 'ArticleList' is simply an ordered collection of articles. It is used 
  to represent an "archive" of blog articles.
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Blog.Types.ArticleList where


import Blog.Prelude 
  ( Eq
  , (<$>), (.)
  )

import Blog.HTML.ArticleList (articleListPageHtml)
import Blog.Types.Article (Article)

import Data.Aeson
  ( FromJSON (parseJSON), ToJSON (toJSON, toEncoding)
  )

import Text.Blaze.Html5 (ToMarkup (toMarkup))



-- ARTICLE LIST 
--------------------------------------------------------------------------------

data ArticleList = ArticleList
  { articleListArticles :: [Article]
  } deriving (Eq)


-- * JSON Representation
--------------------------------------------------------------------------------

instance FromJSON ArticleList where
  parseJSON v = ArticleList <$> parseJSON v


instance ToJSON ArticleList where
  toJSON (ArticleList articles) = toJSON articles
  toEncoding (ArticleList articles) = toEncoding articles


-- * HTML Representation
--------------------------------------------------------------------------------

instance ToMarkup ArticleList where
  toMarkup = articleListPageHtml . articleListArticles


