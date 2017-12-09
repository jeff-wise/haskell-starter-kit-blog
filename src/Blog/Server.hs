
{-| Server
-}


{-# LANGUAGE Arrows #-}


module Blog.Server where


import Blog.Prelude (IO, ($), (<|>))

import qualified Blog.Web.Server as Web
import Blog.Types
  ( Env (Env)
  , Parameters (Parameters)
  )

import Data.Maybe (fromMaybe)
import qualified Database.PostgreSQL.Simple.Internal as PG

import System.Environment (lookupEnv)



run :: Parameters -> IO ()
run (Parameters port) = do
  conn <- pgConnection
  let env = Env conn
  Web.run port env


pgConnection :: IO PG.Connection
pgConnection = do
  testPassword <- lookupEnv "POSTGRES_PASSWORD_TEST"
  prodPassword <- lookupEnv "POSTGRES_PASSWORD_PROD"
  let password = fromMaybe "password not found" $ testPassword <|> prodPassword
  PG.connect $ PG.defaultConnectInfo { 
    PG.connectHost     = "blog-db" 
  , PG.connectPassword = password
  }

