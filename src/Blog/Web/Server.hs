
{-| HTTP Server
   
    Implementation of the HTTP server.
-}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}


module Blog.Web.Server (
    run 
  ) where


import Blog.Prelude
  ( Int, IO, return
  , (==), ($), (&&)
  )
import Blog.Config (staticDirPath)
import Blog.Types (Env)
import Blog.Types.ArticleList ()
import Blog.Types.User (User (User))
import qualified Blog.Web.Handler as Handler
import Blog.Web.API as API (API)
import Blog.Web.Types (SiteHandler)

import Control.Monad.Trans.Reader (runReaderT)

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Servant 
  ( Server, Proxy (Proxy), Raw, Handler
  , (:<|>) ((:<|>)), (:>), (:~>) (NT)
  , enter
  , Context ((:.), EmptyContext), BasicAuthData (BasicAuthData)
  , BasicAuthCheck (BasicAuthCheck)
  , serveWithContext
  , BasicAuthResult (Authorized, Unauthorized)
  )
import Servant.Utils.StaticFiles (serveDirectoryFileServer)



type ServerAPI = API :<|> "static" :> Raw


readerToHandler' :: Env -> (forall a. SiteHandler a -> Handler a)
readerToHandler' env r = runReaderT r env


readerToHandler :: Env -> SiteHandler :~> Handler
readerToHandler env = NT $ readerToHandler' env


-- | Servant Server
readerServer :: Env -> Server ServerAPI
readerServer env = enter (readerToHandler env) siteServer
              :<|> serveDirectoryFileServer staticDirPath
  where 
    sitePublic = Handler.home
            :<|> Handler.getArticle
            :<|> Handler.getImage
    sitePrivate _ = Handler.postArticle
               :<|> Handler.putArticle
               :<|> Handler.postImage
               :<|> Handler.putImage
    siteServer = sitePublic :<|> sitePrivate


authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "user" && password == "password"
        then return (Authorized User)
        else return Unauthorized
  in BasicAuthCheck check


basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext


-- | WAI Application
application :: Env -> Wai.Application
application env = serveWithContext (Proxy :: Proxy ServerAPI) 
                                   basicAuthServerContext
                                   (readerServer env)


-- | Run HTTP Server
run :: Int -> Env -> IO ()
run port env = do
  Warp.run port (application env)

