{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Version (showVersion)
import Distribution.PackageDescription (PackageDescription(..), defaultExtensions, libBuildInfo)
import Distribution.Simple (defaultMainWithHooks, UserHooks(..), simpleUserHooks)
import Distribution.Simple (packageVersion)
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo())
import Distribution.Simple.Setup (BuildFlags(buildVerbosity), fromFlag)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose)
import Distribution.Text (display)
import Distribution.Verbosity (Verbosity)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
      generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
      buildHook simpleUserHooks pkg lbi hooks flags
  }

git :: [String] -> IO String
git args = do
  (code, out, _) <- readProcessWithExitCode "git" args ""
  pure $ case code of
    ExitSuccess -> dropWhileEnd isSpace out
    ExitFailure{} -> "Unknown"

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir

  gitTag <- git ["describe", "--dirty", "--tags", "--all"]
  gitCommit <- git ["rev-parse", "HEAD"]
  gitDate <- git ["log", "HEAD", "-1", "--format=%cd"]
  --buildTime <- readProcess "date" ["-R"] ""

  let exts = maybe [] (map display . defaultExtensions . libBuildInfo) (library pkg)

  writeFile (dir </> "Build_chili.hs") $ unlines $
    [ "module Build_chili where"
    , ""
    , "import Chili.Intro"
    , ""
    , "gitCommit :: String"
    , "gitCommit = " ++ show gitCommit
    , ""
    , "gitTag :: String"
    , "gitTag = " ++ show gitTag
    , ""
    , "gitDate :: String"
    , "gitDate = " ++ show gitDate
    , ""
    -- Think about this because of reproducible builds
--    , "buildTime :: String"
--    , "buildTime = " ++ show buildTime
--    , ""
    , "chiliVersion :: String"
    , "chiliVersion = " ++ show (showVersion $ packageVersion pkg)
    , ""
    , "languageExtensions :: [String]"
    , "languageExtensions = " ++ show exts
    ]
