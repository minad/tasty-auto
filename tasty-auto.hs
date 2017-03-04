import Control.Monad (when)
import Data.List (foldl')
import System.Console.GetOpt (getOpt, usageInfo, ArgDescr(..), ArgOrder(..), OptDescr(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Tasty.Auto

options :: [OptDescr ((String, [String], Bool) -> (String, [String], Bool))]
options =
  [ Option [] ["module"]     (ReqArg (\x (_, b, c) -> (x, b, c))        "MODULE")     "Qualified module name"
  , Option [] ["ingredient"] (ReqArg (\x (a, b, c) -> (a, b ++ [x], c)) "INGREDIENT") "Qualified ingredient name"
  , Option [] ["debug"]      (NoArg  (\   (a, b, _) -> (a, b, True)))                 "Debug output"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    src : _ : dst : optargs
      | (opts, [], []) <- getOpt Permute options optargs -> do
          tests <- findTests src
          let output = showTestDriver modname ingredients src tests ""
              (modname, ingredients, debug) = foldl' (flip id) ("Main", [], False) opts
          when debug $ hPutStrLn stderr output
          writeFile dst output
    _ -> do
      hPutStrLn stderr $ usageInfo "Usage: tasty-auto src _ dst [OPTION...]" options
      exitFailure
