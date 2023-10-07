-- {-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Maybe
import Db
import HugoIncr
import qualified Options.Applicative as OA
import System.Directory (doesDirectoryExist)
import System.FilePath as SF

-- | The database name.
dbName :: String
dbName = ".hugoincr.db"

-- | The default path where the Hugo target can be found.
defaultPath :: String
defaultPath = "."

-- | The default Hugo target name.
defaultTarget :: String
defaultTarget = "public"

-- | Arguments
data Args = Args
  { argsPath :: Maybe String,
    argsTarget :: Maybe String,
    argsReset :: Bool,
    argsVerbose :: Bool
  }

-- | Argument parser.
argsParser :: OA.Parser Args
argsParser =
  Args
    <$> OA.optional
      ( OA.strOption $
          OA.long "path"
            <> OA.metavar "PATH"
            <> OA.help "Subdirectory path where the Hugo public for the increment is located."
      )
    <*> OA.optional
      ( OA.strOption $
          OA.long "target"
            <> OA.metavar "PUBLIC"
            <> OA.help "Hugo public subdirectory name for the increment."
      )
    <*> OA.flag
      False
      True
      ( OA.long "reset"
          <> OA.short 'r'
          <> OA.help "Reset database."
      )
    <*> OA.flag
      False
      True
      ( OA.long "verbose"
          <> OA.short 'v'
          <> OA.help "Enable verbose mode."
      )

-- | Add a version flag to display the program's version.
versionFlag :: OA.Parser (a -> a)
versionFlag = OA.infoOption "0.1.1" (OA.long "version" <> OA.help "Show version")

-- | Add a helper text using the 'helper' function to provide general information about the program.
argsParserWithHelper :: OA.ParserInfo Args
argsParserWithHelper =
  OA.info (argsParser OA.<**> OA.helper OA.<**> versionFlag) (OA.fullDesc <> OA.header "The hugoincr touches files (with a date/time from the previous Hugo build) in your Hugo public directory, so only files that are actually changed get a date/time bump when a hugo build is done. Now an ftp upload that checks files' modified date/time will do an incremental upload.")

-- | Program entry point.
main :: IO ()
main = do
  args <- OA.execParser argsParserWithHelper
  let path = fromMaybe defaultPath (argsPath args)
  let target = fromMaybe defaultTarget (argsTarget args)
  let reset = argsReset args
  let verbose = argsVerbose args
  let ps = [SF.pathSeparator]

  let tp = path ++ ps ++ target
  tpExists <- doesDirectoryExist tp
  if tpExists
    then do
      pool <- connectDb verbose path ps dbName
      if reset
        then resetDb pool verbose
        else do
          migrateDb pool verbose
          incrementTarget pool verbose path ps target
    else putStrLn $ "Hugo target directory " ++ tp ++ " does not exist"
