
{-| Application Types 
   
  -}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Blog.Types where


import Blog.Common (Int)

import qualified Database.PostgreSQL.Simple.Internal as PG



--------------------------------------------------------------------------------
-- CLI PARAMETERS
--------------------------------------------------------------------------------

data Parameters = Parameters
  { parametersPort :: Int
  }


--------------------------------------------------------------------------------
-- SERVER ENVIRONMENT
--------------------------------------------------------------------------------

data Env = Env 
  { envDBConn :: PG.Connection
  }


