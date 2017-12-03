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
import Network.Wreq as Http (post)

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
main = execParser opts >>= runCommand
  where
  opts =
    info (helper <*> commandParser) (
          fullDesc
       <> progDesc ("Run ./dev.hs --help for a list of commands.")
       <> header "Blog Development Commands" )


-- PARAMETER TYPES
--------------------------------------------------------------------------------

data Command =
    CmdBuild BuildParameters
  | CmdRun RunParameters
  | CmdSh ShParameters
  | CmdPSQL
  | CmdInit InitParameters
  deriving (Eq, Show)


data BuildParameters = BuildParameters
  { buildExecutable :: Bool
  , buildAll        :: Bool   
  , buildWebImage   :: Bool
  , buildDBImage    :: Bool
  } deriving (Eq, Show)


data RunParameters = RunParameters
  { runServer :: Bool
  , runDB     :: Bool
  } deriving (Eq, Show)


data ShParameters = ShParameters
  { shServer :: Bool
  , shDB     :: Bool
  } deriving (Eq, Show)


data InitParameters = InitParameters
  { initTestDB :: Bool
  , initProdDB :: Bool
  } deriving (Eq, Show)


-- instance Read DeployCommand where
--   readsPrec _ s = 
--     case s of
--       "images" -> [(UpdateImages, "")]
--       ""       -> [(Update, "")]
--       _        -> []
                      


-- COMMAND LINE PARSERS
--------------------------------------------------------------------------------

-- | Parse the subcommands 
commandParser :: Parser Command
commandParser = hsubparser ( 
     command "build" (info buildParser buildInfo)
  <> command "run" (info _runParser runInfo)
  <> command "sh" (info shParser shInfo)
  <> command "psql" (info psqlParser psqlInfo)
  <> command "init" (info initParser initInfo) )
  where
  buildInfo = (
       fullDesc
    <> progDesc ("Build the [executable, docker images, ...]")
    <> header    "Build Commands" )
  runInfo = (
       fullDesc
    <> progDesc ("Run the [docker container, executable, etc...].")
    <> header    "Run Commands" )
  shInfo = (
       fullDesc
    <> progDesc ("Open a shell in a running docker container.")
    <> header    "Open a shell on the server." )
  psqlInfo = (
       fullDesc
    <> progDesc ("Open PSQL in a running docker container.")
    <> header    "Open PSQL on the server" )
  initInfo = (
       fullDesc
    <> progDesc ("Initialize the database.")
    <> header    "Initialize the database." )


-- | Parse the options for the Service subcommand
buildParser :: Parser Command
buildParser = CmdBuild <$> params
  where
    params = BuildParameters 
         <$> switch
           ( long "web-exe"
          <> short 'x'
          <> help "Build the server executable." )
         <*> switch
           ( long "all"
          <> help "Build executable and local docker images." )
         <*> switch
           ( long "web-image"
          <> short 's'
          <> help "Build the local docker web local image." )
         <*> switch
           ( long "db-image"
          <> short 'd'
          <> help "Build the local docker database image." )


-- | Parse the options for the Service subcommand
_runParser :: Parser Command
_runParser = CmdRun <$> params
  where
    params = RunParameters 
         <$> switch
           ( long "server"
          <> short 's'
          <> help "Run the server container." )
         <*> switch
           ( long "db"
          <> short 'd'
          <> help "Run the database container." )


-- | Parse the options for the Service subcommand
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


-- | Parse the options for the Service subcommand
psqlParser :: Parser Command
psqlParser = pure CmdPSQL


-- | Parse the options for the Service subcommand
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


-- > COMMANDS
--------------------------------------------------------------------------------

runCommand :: Command -> IO ()

runCommand (CmdBuild  params) = do
  -- Executable
  when (buildExecutable params) compileApp
  -- Local Web Image 
  when (buildWebImage params) $ buildServerDockerImage Local
  -- Local DB Image 
  when (buildDBImage params) $ buildDbDockerImage Local
  -- All
  when (buildAll params) $ do
    compileApp
    buildServerDockerImage Local
    buildDbDockerImage Local

runCommand (CmdRun    params) = do
  when (runServer params) runServerDockerContainer
  when (runDB     params) runDbDockerContainer

runCommand (CmdSh     params) = do
  when (shServer params) shServerDockerContainer
  when (shDB     params) shDbDockerContainer

runCommand CmdPSQL           = connectToPSql

runCommand (CmdInit params) = do 
  when (initTestDB params) initializeDatabase
  when (initProdDB params) initializeDatabase



compileApp :: IO ()
compileApp = run (Just ".") "stack" ["build"] "COMPILE APPLICATION"


data BuildEnv = Local | GCloud


-- | Build a new Docker image for a new service executable
buildServerDockerImage :: BuildEnv -> IO ()
buildServerDockerImage buildEnv = do
  -- The name of the application Docker image
  imageName <- appServerImageName buildEnv
  -- Remove the old image (of the same version), if there is one
  (rmExitCode, rmOut, _) <- P.readProcessWithExitCode
                                    "docker" ["rmi", imageName] ""
  when (rmExitCode == ExitSuccess) $ do
    putStrLn "> Remove Old Image"
    putStrLn rmOut
    putStrLn ""
  -- Copy the server executable to the Docker directory
  putStrLn "> Copying new Blog executable..." >> putStrLn ""
  Sh.shelly $ Sh.cp
    -- FROM
    ".stack-work/install/x86_64-linux-dkc84f957ff3bc3d6b09a019caded09bcf/lts-8.12/8.0.2/bin/blog-exe"
    -- TO
    "deploy/docker/server/run"
  -- Copy CSS files
  Sh.shelly $ Sh.rm_rf "deploy/docker/server/css/"
  Sh.shelly $ Sh.cp_r "assets/css" "deploy/docker/server/css/"
  -- Copy SVG files
  Sh.shelly $ Sh.rm_rf "deploy/docker/server/svg/"
  Sh.shelly $ Sh.cp_r "assets/svg" "deploy/docker/server/svg/"
  -- Copy JS files
  Sh.shelly $ Sh.rm_rf "deploy/docker/server/js/"
  Sh.shelly $ Sh.cp_r "js" "deploy/docker/server/js/"
  -- Copy Font files
  Sh.shelly $ Sh.rm_rf "deploy/docker/server/fonts/"
  Sh.shelly $ Sh.cp_r "assets/fonts" "deploy/docker/server/fonts/"
  -- Build the new image
  run (Just "deploy/docker/server")
      "docker" ["build", "-t", imageName, "."]
      "BUILD NEW IMAGE"

-- | Build a new Docker image for a new service executable
buildDbDockerImage :: BuildEnv -> IO ()
buildDbDockerImage buildEnv = do
  -- The name of the application Docker image
  imageName <- appDbImageName buildEnv
  -- Remove the old image (of the same version), if there is one
  -- (rmExitCode, rmOut, _) <- P.readProcessWithExitCode
  --                                   "docker" ["rmi", imageName] ""
  -- when (rmExitCode == ExitSuccess) $ do
  --   putStrLn "> Remove Old Image"
  --   putStrLn rmOut
  --   putStrLn ""
  -- -- Copy the server executable to the Docker directory
  Sh.shelly $ Sh.cp "db/schema.sql" "deploy/docker/db/schema.sql"
  -- Build the new image
  run (Just "deploy/docker/db")
      "docker" ["build", "-t", imageName, "."]
      "BUILD NEW IMAGE"

-- | Run the Docker image
runServerDockerContainer :: IO ()
runServerDockerContainer = do
  -- The docker image name
  imageName <- appServerImageName Local
  -- Remove the containers (if they exist)
  (rmExitCode, rmOut, _) <- P.readProcessWithExitCode
                                    "docker" ["rm", "-f", serverContainerName]
                                    ""
  when (rmExitCode == ExitSuccess) $ do
    putStrLn "> Delete Old Container"
    putStrLn rmOut
  run Nothing
      "docker" ["run",
                "--name",
                serverContainerName,
                "--net=blog", 
                "-p", 
                "8080:80",
                "-d",
                imageName ]
      "RUN CONTAINER"
  return ()


-- | Run the Docker image
runDbDockerContainer :: IO ()
runDbDockerContainer = do
  -- The docker image name
  imageName <- appDbImageName Local
  -- Remove the containers (if they exist)
  (rmExitCode, rmOut, _) <- P.readProcessWithExitCode
                                    "docker" ["rm", "-f", dbContainerName]
                                    ""
  when (rmExitCode == ExitSuccess) $ do
    putStrLn "> Delete Old Container"
    putStrLn rmOut
  run Nothing
      "docker" ["run",
                "--name",
                dbContainerName,
                "--net=blog", 
                "-d",
                imageName ]
      "RUN CONTAINER"
  return ()


pushGCloudImage :: Container ->  IO ()
pushGCloudImage container = do
  containerImageName <- case container of
                          DbContainer -> appDbImageName GCloud
                          ServerContainer -> appServerImageName GCloud
  let cmd = "gcloud docker -- push " <> containerImageName
  run Nothing cmd [] cmdName
  where
    cmdName = "PUSH GCLOUD " 
                 <> (map toUpper $ show container) 
                 <> " IMAGE"


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


initializeDatabase :: IO ()
initializeDatabase = do
  -- (1) Gather the test article file paths.
  let articlesDirPath = dataDirPath </> "articles"
  articleFileNames <- listDirectory articlesDirPath
  let articleFilePaths = fmap (articlesDirPath </>) articleFileNames
  print articleFilePaths
  newArticles <- catMaybes <$> (forM articleFilePaths Yaml.decodeFile :: IO [Maybe Yaml.Value])
  responses <- forM newArticles $ Http.post "http://0.0.0.0:8080/articles"
  putStrLn $ "Added " <> (show $ length responses) <> " to the database."
  

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

appServerImageName :: BuildEnv -> IO String
appServerImageName Local = do
  version <- versionText
  return $ "blog/server:" ++ version
appServerImageName GCloud = do
  version <- versionText
  return $ "us.gcr.io/exalted-shape-178804/site-server:" ++ version

appDbImageName :: BuildEnv -> IO String
appDbImageName Local = do
  version <- versionText
  return $ "blog/db:" ++ version
appDbImageName GCloud = do
  version <- versionText
  return $ "us.gcr.io/exalted-shape-178804/site-pg:" ++ version

serverContainerName :: String
serverContainerName = "blog-server"

dbContainerName :: String
dbContainerName = "blog-db"

callProcessAndPrint :: FilePath -> [String] -> IO ()
callProcessAndPrint cmd args = do
  putStrLn $ (L.intercalate " " $ cmd:args)
  P.callProcess cmd args

run :: Maybe FilePath -> FilePath -> [String] -> String -> IO ()
run mDir cmd args name = do
  putStrLn name
  -- Print the command that is being run, especially so it can be used manually
  -- if needed
  putStrLn $ "Command: " ++ (L.intercalate " " $ cmd:args)
  putStrLn ""
  let process = (P.proc cmd args) { P.cwd = mDir }
  P.readCreateProcess process  "" >> return ()


--------------------------------------------------------------------------------
-- CONFIGURATION
--------------------------------------------------------------------------------


dbImageVersion :: String 
dbImageVersion = "0.7"

webImageVersion :: String 
webImageVersion = "0.7"


dataDirPath :: FilePath
dataDirPath = "data/"
