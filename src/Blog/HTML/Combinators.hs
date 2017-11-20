
{-| HTML Combinators
-}


{-# LANGUAGE OverloadedStrings #-}


module Blog.HTML.Combinators where


import Blog.Common ((==))

import Control.Category ((>>>))

import Data.Text (Text)
import qualified Data.Text as T (cons, empty, foldr, toLower)

import Text.Blaze.Html5 (
    AttributeValue
  , toValue
  )


-- | Create an anchor link from a link URL
anchorId :: Text -> AttributeValue
anchorId = T.toLower
         >>> spacesWithDashes
         >>> toValue


-- | Create an anchor link from a link URL
anchorLink :: Text -> AttributeValue
anchorLink = T.toLower
         >>> spacesWithDashes
         >>> T.cons '#'
         >>> toValue


spacesWithDashes :: Text -> Text
spacesWithDashes = T.foldr spaceIsDash T.empty 
  where
    spaceIsDash c t = if c == ' '
                        then '-' `T.cons` t
                        else c `T.cons` t

