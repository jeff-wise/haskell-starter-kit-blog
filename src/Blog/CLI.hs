
{-| Command Line Interface
-}


module Blog.CLI where


import Blog.Prelude
  ( IO
  , (<>), (<$>), (<**>), (>>=)
  )
import qualified Blog.Server as Server (run)
import Blog.Types (Parameters (Parameters))

import Options.Applicative



-- | Run the web server. Read and parse the command line arguments
run :: IO ()
run = execParser opts >>= Server.run
  where
  opts =
    info (parser <**> helper) (
          fullDesc
       <> progDesc "Serves the Tome website."
       <> header "Tome Site Server" )


-- | CLI Parser
parser :: Parser Parameters
parser = Parameters
      <$> option auto
         ( short 'p'
         <> long "port"
         <> metavar "PORT"
         <> value 80
         <> showDefault
         <> help "The port the web server will run on." )

