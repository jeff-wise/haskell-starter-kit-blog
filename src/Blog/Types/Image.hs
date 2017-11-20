
{-| Image
   
  This module defines the Image type and associated instances.
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Blog.Types.Image where


import Blog.Common
  ( Eq, Int
  , (<$>)
  )

import Data.ByteString.Lazy (ByteString)

import Data.Aeson.Types (pairs, typeMismatch)
import Data.Yaml
  ( FromJSON (parseJSON), ToJSON (toJSON, toEncoding)
  , Value (Object)
  , (.:), (.=)
  , object
  )



-- IMAGE
--------------------------------------------------------------------------------

data Image = Image
  { imageId      :: ImageId
  , imageContent :: ImageContent
  } deriving (Eq)


-- Image > Id
--------------------------------------------------------------------------------

newtype ImageId = ImageId
  { getImageId :: Int }
  deriving (Eq, FromJSON, ToJSON)


-- Image > Content
--------------------------------------------------------------------------------

newtype ImageContent = ImageContent
  { getImageContent :: ByteString }
  deriving (Eq)


-- IMAGE METADATA
--------------------------------------------------------------------------------

data ImageMetadata = ImageMetadata
  { imageMetadataId :: ImageId
  } deriving (Eq)


-- JSON Instance
instance FromJSON ImageMetadata where
  parseJSON (Object hm) = ImageMetadata <$> hm .: "id"
  parseJSON invalid     = typeMismatch "ImageMetadata" invalid


instance ToJSON ImageMetadata where
  toJSON (ImageMetadata _id) = object ["id" .= _id]
  toEncoding (ImageMetadata _id) = pairs ("id" .= _id)


-- NEW IMAGE
--------------------------------------------------------------------------------

data NewImage = NewImage
  { newImageContent :: ImageContent
  } deriving (Eq)


