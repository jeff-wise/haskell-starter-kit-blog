
{-| The blog API

  GET  Home

  GET  Article / [Article]
  POST Articles
  PUT  Articles / [Id]

  GET  Images / [Image]
  POST Images
  PUT  Images / [Id]

-}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module Blog.Web.API
  ( API
  ) where


import Blog.Common (Int)
import Blog.Types.Article (Article, NewArticle)
import Blog.Types.ArticleList (ArticleList)
import Blog.Types.Image (ImageMetadata)
import Blog.Types.User (User)

import Codec.Picture.Types (DynamicImage)

import Servant
  ( Get, Post, Put
  , JSON
  , Capture, ReqBody
  , (:<|>), (:>)
  , BasicAuth
  )
import Servant.HTML.Blaze (HTML)
import Servant.JuicyPixels (PNG)
import Servant.Multipart (MultipartData, MultipartForm)
import Servant.Yaml (YAML)



type PublicAPI = 
  -- GET Home
       Get '[JSON,YAML,HTML] ArticleList
  -- GET Article / [Article]
  :<|> "articles" :> Capture "article_id" Int :> Get '[JSON,YAML,HTML] Article
  -- GET Images / [Image]
  :<|> "images" :> Capture "image_id" Int :> Get '[PNG] DynamicImage


type PrivateAPI = 
  -- POST Articles
       "articles" :> ReqBody '[JSON,YAML] NewArticle :> Post '[JSON,YAML] Article
  -- PUT Articles / [Id]
  :<|> "articles" :> Capture "article_id" Int :> ReqBody '[JSON,YAML] Article :> Put '[JSON,YAML] Article
  -- POST Images
  :<|> MultipartForm MultipartData :> "images" :> Post '[JSON,YAML] ImageMetadata
  -- PUT Images / [Id]
  :<|> MultipartForm MultipartData :> "images" :> Capture "image_id" Int :> Put '[JSON,YAML] ImageMetadata


type API = PublicAPI :<|> BasicAuth "edit-realm" User :> PrivateAPI

