
{-| Database Image Schema
   
-}


{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Blog.DB.Image where


import Blog.Common
  ( Int
  , Maybe (Just, Nothing)
  , ($), (.)
  )
import Blog.Types.Image 
  ( Image, imageId, imageContent
  , NewImage, newImageContent
  , getImageId, getImageContent
  )

import Control.Arrow (returnA)

import Data.ByteString.Lazy (ByteString)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import Opaleye
 ( Table (Table), Column
 , PGInt4, PGBytea
 , pgInt4, pgLazyByteString
 , optional, required
 , Query, queryTable, restrict, (.==)
 )



data DB_Image' a b = DB_Image
  { dbImageId      :: a
  , dbImageContent :: b  
  }


type DB_Image = DB_Image' Int ByteString


type ImageRowRead = DB_Image' (Column PGInt4) (Column PGBytea)
type ImageRowWrite = DB_Image' (Maybe (Column PGInt4)) (Column PGBytea)


$(makeAdaptorAndInstance "pImage" ''DB_Image')


imageTable :: Table ImageRowWrite ImageRowRead 
imageTable = Table "image"
                   (pImage $ DB_Image (optional "id"    )
                                      (required "image" ))


allImagesQuery :: Query ImageRowRead
allImagesQuery = queryTable imageTable


imageWithIdQuery :: Int -> Query ImageRowRead
imageWithIdQuery targetId = proc () -> do
  img@(DB_Image rowImageId _) <- allImagesQuery -< ()
  restrict -< rowImageId .== pgInt4 targetId
  returnA -< img


imageNewRow :: NewImage -> ImageRowWrite
imageNewRow newImage = 
  DB_Image Nothing 
           (pgLazyByteString $ getImageContent $ newImageContent newImage)


imageUpdateRow :: Image -> ImageRowWrite
imageUpdateRow image = 
  DB_Image (Just $ pgInt4 $ getImageId $ imageId image)
           (pgLazyByteString $ getImageContent $ imageContent image)
