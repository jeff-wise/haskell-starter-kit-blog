#!/usr/bin/env stack
{- stack
   --resolver lts-9.14
   --install-ghc
   --stack-yaml stack-script.yaml
   runghc
   --package base
   --package blog
   --package Cabal
   --package directory
   --package filepath
   --package http-client
   --package lens
   --package optparse-applicative
   --package process
   --package shelly
   --package wreq
   --package yaml
   --  
   -hide-all-packages
-}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-| Development Process CLI Tool
    
    Manages a Continuous Deployment process for the Tome Site application that 
    includes compiling to an executable and deploying to a Docker container, 
    as well as any utility functions.
-}


{-# LANGUAGE OverloadedStrings #-}


import Blog.Types.Article (Article, NewArticle)
import Blog.Web.API (API)

import Control.Lens ((&), (?~))
import Control.Monad (when)

import Data.Char (toUpper)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.List as L (intercalate)
import Data.Traversable (forM)
import Data.Version (showVersion)
import qualified Data.Yaml as Yaml (decodeFile, Value)

import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (package, packageDescription)
import Distribution.Verbosity (silent)

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wreq (auth, basicAuth)
import qualified Network.Wreq as HTTP
  ( defaults
  , postWith
  )

import Options.Applicative

import qualified Shelly as Sh (
    shelly
  , cp, cp_r
  , rm_rf
  , sleep
  )

import System.Directory (getCurrentDirectory, listDirectory)
import System.Exit (ExitCode (..))
import qualified System.Process as P 
  ( spawnProcess
  , callProcess
  , proc, cwd
  , readCreateProcess, readProcessWithExitCode 
  )
import System.FilePath.Posix ((</>))


-- MAIN
--------------------------------------------------------------------------------

-- | Read and parse the command line arguments
main :: IO ()
main = execParser opts >>= runCommand >> putStrLn ""
  where
  opts =
    info (helper <*> commandParser) (
          fullDesc
       <> progDesc ("Run ./dev.hs --help for a list of commands.")
       <> header "Blog Development Commands" )


-- COMMANDS
--------------------------------------------------------------------------------

data Command =
    -- Build executable or docker images
    CmdBuild BuildParameters
    -- Run executable or image containers
  | CmdRun RunParameters
    -- Build and run application
  | CmdDeploy DeployParameters
    -- Initialize database data
  | CmdInit InitParameters
    -- Sh into running containers
  | CmdSh ShParameters
    -- Run psql on running containers
  | CmdPSQL
  deriving (Eq, Show)


-- | Parse a command
commandParser :: Parser Command
commandParser = hsubparser ( 
     command "build" (info buildParser buildInfo)
  <> command "run" (info _runParser runInfo)
  <> command "sh" (info shParser shInfo)
  <> command "psql" (info psqlParser psqlInfo)
  <> command "init" (info initParser initInfo)
  <> command "deploy" (info deployParser deployInfo) )

  where

    buildInfo = (
         fullDesc
      <> progDesc ("Build the [executable, docker images, ...]")
      <> header    "Build" )
    runInfo = (
         fullDesc
      <> progDesc ("Run the [docker container, executable, etc...].")
      <> header    "Run" )
    shInfo = (
         fullDesc
      <> progDesc ("Run sh/bash in a running docker container.")
      <> header    "Shell" )
    psqlInfo = (
         fullDesc
      <> progDesc ("run Psql in a running docker container.")
      <> header    "PostgreSQL Shell" )
    initInfo = (
         fullDesc
      <> progDesc ("Initialize the database data.")
      <> header    "Initialize the database" )
    deployInfo = (
         fullDesc
      <> progDesc ("Compile, build images, and run containers.")
      <> header    "Deploy the application" )


-- | Perform the actions for a command
runCommand :: Command -> IO ()
runCommand (CmdBuild  params) = do
  -- Executable
  when (buildExecutable params) compileWebExecutable
  -- Local Web Image 
  when (buildWebImage params) buildWebDockerImage
  -- Local DB Image 
  when (buildDBImage params) buildDBDockerImage
  -- All (Explicit)
  when (buildAll params) buildAllDockerImages
  -- All (Implicit)
  let commandGiven = buildExecutable params ||
                     buildWebImage params ||
                     buildDBImage params ||
                     buildAll params
  when (not commandGiven) buildAllDockerImages
runCommand (CmdRun    params) = do
  -- Web
  when (runWeb params) runWebDockerContainer
  -- DB
  when (runDB  params) runDBDockerContainer
  -- All (Explicit)
  when (runAll params) runAllDockerContainers
  -- All (Implicit)
  let commandGiven = runWeb params || runDB params || runAll params
  when (not commandGiven) runAllDockerContainers
runCommand (CmdDeploy params) = do
  -- Web
  when (deployWeb params) deployWebContainer
  -- Database
  when (deployDB params) deployDBContainer
  -- All (Explicit)
  when (deployAll params) deployAllContainers
  -- All (Implicit)
  let commandGiven = deployWeb params ||
                     deployDB params ||
                     deployAll params
  when (not commandGiven) deployAllContainers
runCommand (CmdInit params) = do 
  when (initTestDB params) initializeDatabase
  when (initProdDB params) initializeDatabase
  when (not $ initProdDB params || initTestDB params) $
    putStrLn "Please specify an environment: --test or --prod"
runCommand (CmdSh     params) = do
  when (shServer params) shServerDockerContainer
  when (shDB     params) shDbDockerContainer
runCommand CmdPSQL           = connectToPSql

-- Commands > Build
--------------------------------------------------------------------------------

data BuildParameters = BuildParameters
  { buildExecutable :: Bool
  , buildWebImage   :: Bool
  , buildDBImage    :: Bool
  , buildAll        :: Bool   
  } deriving (Eq, Show)


-- | Parse a build command
buildParser :: Parser Command
buildParser = CmdBuild <$> params
  where
    params = BuildParameters 
         <$> switch
           ( long "web-exe"
          <> help "Build the server executable." )
         <*> switch
           ( long "web"
          <> help "Build the local docker web local image." )
         <*> switch
           ( long "db"
          <> help "Build the local docker database image." )
         <*> switch
           ( long "all"
          <> help "Build executable and local docker images." )

-- Commands > Run
--------------------------------------------------------------------------------

data RunParameters = RunParameters
  { runWeb :: Bool
  , runDB     :: Bool
  , runAll    :: Bool
  } deriving (Eq, Show)

-- | Parse a run command
_runParser :: Parser Command
_runParser = CmdRun <$> params
  where
    params = RunParameters 
         <$> switch
           ( long "web"
          <> help "Run the server container." )
         <*> switch
           ( long "db"
          <> help "Run the database container." )
         <*> switch
           ( long "all"
          <> help "Run all containers." )

-- Commands > Deploy
--------------------------------------------------------------------------------

data DeployParameters = DeployParameters
  { deployWeb :: Bool
  , deployDB  :: Bool
  , deployAll :: Bool
  } deriving (Eq, Show)

-- | Parse a deploy command
deployParser :: Parser Command
deployParser = CmdDeploy <$> params
  where
    params = DeployParameters
         <$> switch
           ( long "web"
          <> help "Deploy the web container." )
         <*> switch
           ( long "db"
          <> help "Deploy the database container." )
         <*> switch
           ( long "all"
          <> help "Deploy the database and web containers." )

-- Commands > Init
--------------------------------------------------------------------------------

data InitParameters = InitParameters
  { initTestDB :: Bool
  , initProdDB :: Bool
  } deriving (Eq, Show)

-- | Parse an init command
initParser :: Parser Command
initParser = CmdInit <$> params
  where
    params = InitParameters
         <$> switch
           ( long "test"
          <> short 't'
          <> help "Initialize the TEST database." )
         <*> switch
           ( long "prod"
          <> short 'p'
          <> help "Initialize the PROD database." )

-- Commands > Sh
--------------------------------------------------------------------------------

data ShParameters = ShParameters
  { shServer :: Bool
  , shDB     :: Bool
  } deriving (Eq, Show)

-- | Parse an sh command
shParser :: Parser Command
shParser = CmdSh <$> params
  where
    params = ShParameters 
         <$> switch
           ( long "server"
          <> short 's'
          <> help "Run the server container." )
         <*> switch
           ( long "db"
          <> short 'd'
          <> help "Run the database container." )

-- Commands > PSQL
--------------------------------------------------------------------------------

-- | Parse a PSQL command 
psqlParser :: Parser Command
psqlParser = pure CmdPSQL


-- TASKS
--------------------------------------------------------------------------------

-- Tasks > Compilation
--------------------------------------------------------------------------------

-- | Compile the executable for the web server
compileWebExecutable :: IO ()
compileWebExecutable = do
  run "." "stack" ["build"] "COMPILE WEB SERVER"

-- Tasks > Build
--------------------------------------------------------------------------------

-- | Compile the server executable and then build the web and db images
buildAllDockerImages :: IO ()
buildAllDockerImages = do
  compileWebExecutable
  run "." "docker-compose" ["build"] "BUILD ALL IMAGES"

-- | Build a new Docker image for a new service executable
buildWebDockerImage :: IO ()
buildWebDockerImage = do
  compileWebExecutable
  prepareWebImage 
  -- Build the new image
  run "." "docker-compose" ["build", "--no-cache", "web"] "BUILD WEB IMAGE"

-- | Build a new Docker image for a new service executable
buildDBDockerImage :: IO ()
buildDBDockerImage = do
  prepareDBImage 
  -- Build the new image
  run "." "docker-compose" ["build", "--no-cache", "db"] "BUILD DATABASE IMAGE"


-- Tasks > Run
--------------------------------------------------------------------------------

-- | Run the Web docker container
runWebDockerContainer :: IO ()
runWebDockerContainer = do
  run "." "docker-compose" ["up", "--no-build", "--no-deps", "--force-recreate", "-d", "web"] "RUN WEB CONTAINER"

-- | Run the DB docker container
runDBDockerContainer :: IO ()
runDBDockerContainer = do
  run "." "docker-compose" ["stop", "db"] "STOP DATABASE CONTAINER"
  run "." "docker-compose" ["rm", "--force", "-v", "db"] "REMOVE DATABASE CONTAINER / VOLUME"
  run "." "docker-compose" ["up", "--no-build", "--force-recreate", "-d", "db"] "RUN DATABASE CONTAINER"

-- | Run all docker containers
runAllDockerContainers :: IO ()
runAllDockerContainers = do
  run "." "docker-compose" ["restart"] "RUN ALL CONTAINERS"


-- Tasks > Deploy
--------------------------------------------------------------------------------

-- | Compile the web server executable, copy files, build the image, 
-- and run the container
deployWebContainer :: IO ()
deployWebContainer = do
  buildWebDockerImage
  runWebDockerContainer
  -- run "." "docker-compose" ["up", "-d", "--no-deps", "--build", "web"] 
  --     "DEPLOY WEB SERVER"

-- | Copy database files, build the image, and run the container 
deployDBContainer :: IO ()
deployDBContainer = do
  buildDBDockerImage
  runDBDockerContainer
  -- run "." "docker-compose" ["up", "-d", "--no-deps", "--build", "db"]
  --   "DEPLOY DATABASE"

-- | Compile the server executable, build the images, and run the containers
deployAllContainers :: IO ()
deployAllContainers = do
  compileWebExecutable
  run "." "docker-compose" ["up", "-d"]
    "DEPLOY BLOG"


-- Tasks > Sh
--------------------------------------------------------------------------------

-- | Connect to a running container's bash shell
shServerDockerContainer :: IO ()
shServerDockerContainer =
  callProcessAndPrint "docker" [
                      "exec",
                      "-it",
                      serverContainerName,
                      "/bin/sh" ]

-- | Connect to a running container's bash shell
shDbDockerContainer :: IO ()
shDbDockerContainer =
  callProcessAndPrint "docker" [
                      "exec",
                      "-it",
                      dbContainerName,
                      "/bin/sh" ]


-- Tasks > Psql
--------------------------------------------------------------------------------

-- | Connect to a running container's psql shell
connectToPSql :: IO ()
connectToPSql =
  callProcessAndPrint "docker" [
                      "exec",
                      "-it",
                      dbContainerName,
                      "psql",
                      "-U",
                      "postgres" ]


-- Tasks > Initialize Database
--------------------------------------------------------------------------------

initializeDatabase :: IO ()
initializeDatabase = do
  -- (1) Gather the test article file paths.
  let articlesDirPath = dataDirPath </> "articles"
  articleFileNames <- listDirectory articlesDirPath
  let articleFilePaths = fmap (articlesDirPath </>) articleFileNames
  print articleFilePaths
  newArticles <- catMaybes <$> (forM articleFilePaths Yaml.decodeFile :: IO [Maybe Yaml.Value])
  let opts = HTTP.defaults & auth ?~ basicAuth "user" "password"
  responses <- forM newArticles $ HTTP.postWith opts "http://0.0.0.0:8080/articles"
  putStrLn $ "Added " <> (show $ length responses) <> " articles to the database."


-- Tasks > WEB
--------------------------------------------------------------------------------

-- | Prepare files needed for the Web server image
prepareWebImage :: IO ()
prepareWebImage = do
  putStrLn "> Copying new executable..."
  Sh.shelly $ Sh.cp
    -- FROM
    ".stack-work/install/x86_64-linux-dkc84f957ff3bc3d6b09a019caded09bcf/lts-9.14/8.0.2/bin/blog-exe"
    -- TO
    "deploy/docker/web/run"
  -- Copy CSS files
  putStrLn "> Copying CSS files..."
  Sh.shelly $ Sh.rm_rf "deploy/docker/web/css/"
  Sh.shelly $ Sh.cp_r "assets/css" "deploy/docker/web/css/"
  -- Copy SVG files
  putStrLn "> Copying SVG files..."
  Sh.shelly $ Sh.rm_rf "deploy/docker/web/svg/"
  Sh.shelly $ Sh.cp_r "assets/svg" "deploy/docker/web/svg/"
  -- Copy JS files
  putStrLn "> Copying JS files..."
  Sh.shelly $ Sh.rm_rf "deploy/docker/web/js/"
  Sh.shelly $ Sh.cp_r "js" "deploy/docker/web/js/"
  -- Copy Font files
  putStrLn "> Copying Font files..."
  Sh.shelly $ Sh.rm_rf "deploy/docker/web/fonts/"
  Sh.shelly $ Sh.cp_r "assets/fonts" "deploy/docker/web/fonts/"

-- | Prepare files needed for the database image
prepareDBImage :: IO ()
prepareDBImage = do
  Sh.shelly $ Sh.cp "db/schema.sql" "deploy/docker/db/schema.sql"


-- HELPERS
--------------------------------------------------------------------------------

data Container = 
    DbContainer 
  | ServerContainer


instance Show Container where
  show DbContainer     = "Database"
  show ServerContainer = "Server"


versionText :: IO String
versionText = do
  pkgDesc <- readPackageDescription silent "blog.cabal"
  return $ showVersion $ pkgVersion $ package $ packageDescription pkgDesc 

-- appWebImageName :: IO String
-- appWebImageName = do
--   version <- versionText
--   return $ "blog/web:" ++ version

-- appDbImageName :: IO String
-- appDbImageName = do
--   version <- versionText
--   return $ "blog/db:" ++ version

serverContainerName :: String
serverContainerName = "blog-web"

dbContainerName :: String
dbContainerName = "blog-db"

callProcessAndPrint :: FilePath -> [String] -> IO ()
callProcessAndPrint cmd args = do
  putStrLn $ (L.intercalate " " $ cmd:args)
  P.callProcess cmd args

run :: FilePath -> FilePath -> [String] -> String -> IO ()
run dir cmd args headerString = do
  putStrLn ""
  printTaskHeader headerString
  putStrLn $ "Command: " <> (L.intercalate " " $ cmd:args)
  putStrLn $ "-------------------------------------------------------------------------"
  let process = (P.proc cmd args) { P.cwd = Just dir }
  P.readCreateProcess process  "" >> return ()

printTaskHeader :: String -> IO ()
printTaskHeader headerString = 
  putStrLn $ yellow <> headerString <> no_color
  where
    yellow = "\x1B[33m"
    no_color="\x1B[0m"

--------------------------------------------------------------------------------
-- CONFIGURATION
--------------------------------------------------------------------------------

dataDirPath :: FilePath
dataDirPath = "data/"
