import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Test.Tasty.Auto

main :: IO ()
main = do
  args <- getArgs
  case args of
    src : _ : dst : _ -> do
      tests <- findTests src
      writeFile dst $ showTestDriver src tests ""
    _ -> do
      hPutStrLn stderr "tasty-auto: Expected source and destination arguments"
      exitFailure
