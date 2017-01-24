module Test.Tasty.Auto (findTests, showTestDriver) where

import Data.Function (on)
import Data.List (find, isPrefixOf, isSuffixOf, nub, intersperse, groupBy, sortOn)
import Data.Maybe (fromJust)
import System.Directory (listDirectory, doesDirectoryExist)
import Data.Traversable (for)
import System.FilePath ((</>), takeDirectory, pathSeparator, dropExtension)
import Data.Monoid (Endo(..))
import Data.Foldable (fold)

data Generator = Generator
  { genPrefix :: String
  , genImport :: ShowS
  , genInstance :: ShowS
  , genSetup :: Test -> ShowS }

data Test = Test { testModule, testFunction :: String }

str :: String -> ShowS
str = (++)

sp, nl :: ShowS
sp = (' ':)
nl = ('\n':)

tr :: Char -> Char -> String -> String
tr a b = map $ \c -> if c == a then b else c

name, fn, var :: Test -> ShowS
name = shows . tr '_' ' ' . tail . dropWhile (/= '_') . testFunction
fn t = str (testModule t) . ('.':) . str (testFunction t)
var t = str "setup_" . str (tr '.' '_' $ testModule t) . ('_':) . str (testFunction t)

generators :: [Generator]
generators =
  [ Generator { genPrefix = "prop_"
              , genImport = str "import qualified Test.Tasty.QuickCheck\n"
              , genInstance = id
              , genSetup = \t -> str "pure $ Test.Tasty.QuickCheck.testProperty " . name t . sp . fn t }
  , Generator { genPrefix = "scprop_"
              , genImport = str "import qualified Test.Tasty.SmallCheck\n"
              , genInstance = id
              , genSetup = \t -> str "pure $ Test.Tasty.SmallCheck.testProperty " . name t . sp . fn t }
  , Generator { genPrefix = "case_"
              , genImport = str "import qualified Test.Tasty.HUnit\n"
              , genInstance =
                  str "class TestCase a where testCase :: String -> a -> IO Test.Tasty.TestTree\n"
                . str "instance TestCase (IO ())                      where testCase n = pure . Test.Tasty.HUnit.testCase      n\n"
                . str "instance TestCase (IO String)                  where testCase n = pure . Test.Tasty.HUnit.testCaseInfo  n\n"
                . str "instance TestCase ((String -> IO ()) -> IO ()) where testCase n = pure . Test.Tasty.HUnit.testCaseSteps n\n"
              , genSetup = \t -> str "testCase " . name t . sp . fn t }
  , Generator { genPrefix = "spec_"
              , genImport = str "import qualified Test.Tasty.Hspec\n"
              , genInstance = id
              , genSetup = \t -> str "Test.Tasty.Hspec.testSpec " . name t . sp . fn t }
  , Generator { genPrefix = "test_"
              , genImport = id
              , genInstance =
                  str "class TestGroup a where testGroup :: String -> a -> IO Test.Tasty.TestTree\n"
                . str "instance TestGroup Test.Tasty.TestTree          where testGroup _ a = pure a\n"
                . str "instance TestGroup [Test.Tasty.TestTree]        where testGroup n a = pure $ Test.Tasty.testGroup n a\n"
                . str "instance TestGroup (IO Test.Tasty.TestTree)     where testGroup _ a = a\n"
                . str "instance TestGroup (IO [Test.Tasty.TestTree])   where testGroup n a = Test.Tasty.testGroup n <$> a\n"
              , genSetup = \t -> str "testGroup " . name t . sp . fn t } ]

testFileSuffixes :: [String]
testFileSuffixes = (++) <$> ["Spec", "Test"] <*> [".lhs", ".hs"]

getGenerator :: Test -> Generator
getGenerator t = fromJust $ find ((`isPrefixOf` testFunction t) . genPrefix) generators

getGenerators :: [Test] -> [Generator]
getGenerators = map head . groupBy  ((==) `on` genPrefix) . sortOn genPrefix . map getGenerator

showImports :: [Test] -> ShowS
showImports = foldEndo . map (\m -> str "import qualified " . str m . nl) . nub . map testModule

showSetup :: Test -> ShowS
showSetup t = str "  " . var t . str " <- " . genSetup (getGenerator t) t . nl

foldEndo :: (Functor f, Foldable f) => f (a -> a) -> (a -> a)
foldEndo = appEndo . fold . fmap Endo

showTestDriver :: FilePath -> [Test] -> ShowS
showTestDriver src ts = let gs = getGenerators ts in
    str "{-# LINE 1 " . shows src . str " #-}\n"
  . str "{-# LANGUAGE FlexibleInstances #-}\n"
  . str "module Main where\n"
  . str "import Prelude\n"
  . str "import qualified Test.Tasty\n"
  . foldEndo (map genImport gs)
  . showImports ts
  . foldEndo (map genInstance gs)
  . str "main :: IO ()\n"
  . str "main = do\n"
  . foldEndo (map showSetup ts)
  . str "  Test.Tasty.defaultMain $ Test.Tasty.testGroup " . shows src
  . str "\n    [ "
  . foldEndo (intersperse (str "\n    , ") $ map var ts)
  . str " ]\n"

filesBySuffix :: FilePath -> [String] -> IO [FilePath]
filesBySuffix dir suffixes = do
  entries <- listDirectory dir
  found <- for entries $ \entry -> do
    let dir' = dir </> entry
    exists <- doesDirectoryExist dir'
    if exists then map (entry </>) <$> filesBySuffix dir' suffixes else pure []
  pure $ filter (\x -> any (`isSuffixOf` x) suffixes) entries ++ concat found

findTests :: FilePath -> IO [Test]
findTests src = do
  let dir = takeDirectory src
  files <- filesBySuffix dir testFileSuffixes
  concat <$> traverse (\f -> extractTests f <$> readFile (dir </> f)) files

mkTest :: FilePath -> String -> Test
mkTest = Test . tr pathSeparator '.' . dropExtension

extractTests :: FilePath -> String -> [Test]
extractTests file =
    map (mkTest file) . nub
  . filter (\n -> any ((`isPrefixOf` n) . genPrefix) generators)
  . map fst . concatMap lex . lines
