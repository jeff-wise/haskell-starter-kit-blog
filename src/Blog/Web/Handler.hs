
{-| API Handlers
   
    Implementations of the API's methods.
-}


{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Blog.Web.Handler where


import Blog.Common
  ( Int, Int64, show
  , Maybe (Just, Nothing)
  , Either (Left, Right)
  , IO, liftIO
  , return
  , ($), (<>), (/=)
  , id
  )
import Blog.Types (envDBConn)
import Blog.DB.Article
  ( articleTable
  , articleNewRow, articleUpdateRow
  , articleWithIdQuery
  , dbArticleId
  , articleFromDB
  , DB_Article
  )
import Blog.DB.Image
  ( imageTable
  , imageNewRow, imageUpdateRow
  , imageWithIdQuery
  , dbImageId, dbImageContent
  , DB_Image
  )
import Blog.Web.Types (SiteHandler)
import Blog.Types.Article 
  ( NewArticle, Article )
import Blog.Types.ArticleList (ArticleList (ArticleList))
import Blog.Types.Image
  ( Image (Image)
  , NewImage (NewImage)
  , ImageMetadata (ImageMetadata)
  , ImageId (ImageId)
  , ImageContent (ImageContent)
  )

import Codec.Picture.Png (decodePng)
import Codec.Picture.Types (DynamicImage)

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)

import qualified Database.PostgreSQL.Simple.Internal as PG (Connection)

import qualified Data.ByteString.Lazy as BSL (readFile, toStrict)
import qualified Data.ByteString.Lazy.Char8 as BSL (pack)

import qualified Opaleye as PG
  ( runQuery
  , runInsertManyReturning, runInsertMany
  , runUpdate
  , constant
  )
import Opaleye ((.==))

import Servant.Multipart (MultipartData, lookupFile, fdFilePath)
import Servant.Server 
  ( err400, err404
  , errBody
  )



--------------------------------------------------------------------------------
-- HOME
--------------------------------------------------------------------------------

home :: SiteHandler ArticleList
home = return $ ArticleList []
 

--------------------------------------------------------------------------------
-- ARTICLES
--------------------------------------------------------------------------------

-- GET Articles / [Id]
--------------------------------------------------------------------------------

getArticle :: Int -> SiteHandler Article
getArticle articleId = do
  env <- ask
  let query = articleWithIdQuery articleId
  dbArticles <- liftIO $ PG.runQuery (envDBConn env) query 
  case dbArticles of
    []  -> throwError $ err404 { errBody = "article not found" }
    dbArticle:_ -> return $ articleFromDB dbArticle


-- POST Articles
--------------------------------------------------------------------------------

postArticle :: NewArticle -> SiteHandler Article
postArticle newArticle = do
  env <- ask
  insertedDBArticles <- liftIO $ insertRow $ envDBConn env
  case insertedDBArticles of
    []          -> throwError $ err400 { 
                      errBody = "no articles inserted" }
    dbArticle:_ -> return $ articleFromDB dbArticle
  where
    insertRow :: PG.Connection -> IO [DB_Article]
    insertRow conn = do
      newArticleRow <- articleNewRow newArticle
      PG.runInsertManyReturning conn articleTable [newArticleRow] id


-- PUT Articles / [Id]
--------------------------------------------------------------------------------

putArticle :: Int -> Article -> SiteHandler Article
putArticle articleId article = do
  env <- ask
  rowsUpdated <- liftIO $ updateRow $ envDBConn env
  if (rowsUpdated /= 0)
     then return article
     else throwError $ err400 { 
        errBody = "article does not exist: " <> (BSL.pack $ show articleId) }
  where
    updateRow :: PG.Connection -> IO Int64
    updateRow conn = do
      let rowUpdate _ = articleUpdateRow article
          rowSelector dbArticle = 
            dbArticleId dbArticle .== PG.constant articleId
      PG.runUpdate conn         -- Connection
                   articleTable -- Table 
                   rowUpdate    -- Update function
                   rowSelector  -- Which rows to update.

--------------------------------------------------------------------------------
-- IMAGES
--------------------------------------------------------------------------------

-- GET Images / [Id]
--------------------------------------------------------------------------------

getImage :: Int -> SiteHandler DynamicImage
getImage imageId = do
  env <- ask
  let query = imageWithIdQuery imageId
  dbImages <- liftIO $ PG.runQuery (envDBConn env) query :: SiteHandler [DB_Image]
  case dbImages of
    dbImage:[] -> do
      case decodePng $ BSL.toStrict $ dbImageContent dbImage of
        Left errString -> 
          throwError $ err404 { errBody = "Could not parse image.\n\n" <> BSL.pack errString }
        Right image -> return image
    []         -> throwError $ err404 { errBody = "Image does not exist." }
    _          -> throwError $ err400


-- POST Images
--------------------------------------------------------------------------------

postImage :: MultipartData -> SiteHandler ImageMetadata
postImage multipartData = do
  env <- ask
  case lookupFile "image" multipartData of
    Just imageData -> do
      imageBS <- liftIO $ BSL.readFile $ fdFilePath imageData
      let newImage = NewImage $ ImageContent imageBS
      insertedImageIds <- liftIO $ insertRow (envDBConn env) newImage
      case insertedImageIds of
        []                -> throwError $ err400 { 
                               errBody = "no articles inserted" }
        insertedImageId:_ -> return $ ImageMetadata $ ImageId insertedImageId
    Nothing -> throwError $ err400 { 
                 errBody = "Image data not found" }
  where
    insertRow :: PG.Connection -> NewImage -> IO [Int]
    insertRow conn newImage =
      PG.runInsertManyReturning conn
                                imageTable 
                                [imageNewRow newImage]
                                dbImageId


-- PUT Images / [Id]
--------------------------------------------------------------------------------

putImage :: MultipartData -> Int -> SiteHandler ImageMetadata
putImage multipartData imageId = do
  env <- ask
  case lookupFile "image" multipartData of
    Just imageData -> do
      imageBS <- liftIO $ BSL.readFile $ fdFilePath imageData
      let image = Image (ImageId imageId) (ImageContent imageBS)
      rowsUpdated <- liftIO $ PG.runInsertMany (envDBConn env)
                                           imageTable 
                                           [imageUpdateRow image]       
      return $ ImageMetadata $ ImageId imageId
    Nothing -> throwError $ err400 { 
                 errBody = "Image not found with id: " <> (BSL.pack $ show imageId) }

