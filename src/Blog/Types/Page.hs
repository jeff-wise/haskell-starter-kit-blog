
{-| Page
-}


module Blog.Types.Page where


import Blog.Prelude (Int)

import Data.Text (Text)



data Page =
    PageHome
  | PageArticle ArticlePage
  | PageAbout


data ArticlePage = ArticlePage
  { articlePageId    :: Int
  , articlePageTitle :: Text
  , articlePageDate  :: Text
  }

