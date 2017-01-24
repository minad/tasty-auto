module Test.Tasty.Auto (findTests, showTestDriver) where

import Data.Function (on)
import Data.List (find, isPrefixOf, isSuffixOf, nub, intersperse, groupBy, sortOn)
import Data.Maybe (fromJust)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Data.Traversable (for)
import System.FilePath ((</>), takeDirectory, pathSeparator, dropExtension)
import Data.Monoid (Endo(..))
import Data.Foldable (fold)

data Generator = Generator
  { genPrefix :: String
  , genImport, genClass :: ShowS
  , genSetup :: Test -> ShowS }

data Test = Test { testModule, testFunction :: String }

str :: String -> ShowS
str = (++)

sp, nl :: ShowS
sp = (' ':)
nl = ('\n':)

tr :: Char -> Char -> String -> String
tr a b = map $ \c -> if c == a then b else c

name, fn :: Test -> ShowS
name = shows . tr '_' ' ' . tail . dropWhile (/= '_') . testFunction
fn t = str (testModule t) . ('.':) . str (testFunction t)

generators :: [Generator]
generators =
  [ Generator { genPrefix = "prop_"
              , genImport = str "import qualified Test.Tasty.QuickCheck as QC\n"
              , genClass  = id
              , genSetup  = \t -> str "pure $ QC.testProperty " . name t . sp . fn t }
  , Generator { genPrefix = "scprop_"
              , genImport = str "import qualified Test.Tasty.SmallCheck as SC\n"
              , genClass  = id
              , genSetup  = \t -> str "pure $ SC.testProperty " . name t . sp . fn t }
  , Generator { genPrefix = "case_"
              , genImport = str "import qualified Test.Tasty.HUnit as HU\n"
              , genClass  =
                  str "class TestCase a where testCase :: String -> a -> IO T.TestTree\n"
                . str "instance TestCase (IO ())                      where testCase n = pure . HU.testCase      n\n"
                . str "instance TestCase (IO String)                  where testCase n = pure . HU.testCaseInfo  n\n"
                . str "instance TestCase ((String -> IO ()) -> IO ()) where testCase n = pure . HU.testCaseSteps n\n"
              , genSetup  = \t -> str "testCase " . name t . sp . fn t }
  , Generator { genPrefix = "spec_"
              , genImport = str "import qualified Test.Tasty.Hspec as HS\n"
              , genClass  = id
              , genSetup  = \t -> str "HS.testSpec " . name t . sp . fn t }
  , Generator { genPrefix = "test_"
              , genImport = id
              , genClass  =
                  str "class TestGroup a where testGroup :: String -> a -> IO T.TestTree\n"
                . str "instance TestGroup T.TestTree        where testGroup _ a = pure a\n"
                . str "instance TestGroup [T.TestTree]      where testGroup n a = pure $ T.testGroup n a\n"
                . str "instance TestGroup (IO T.TestTree)   where testGroup _ a = a\n"
                . str "instance TestGroup (IO [T.TestTree]) where testGroup n a = T.testGroup n <$> a\n"
              , genSetup  = \t -> str "testGroup " . name t . sp . fn t } ]

testFileSuffixes :: [String]
testFileSuffixes = (++) <$> ["Spec", "Test"] <*> [".lhs", ".hs"]

getGenerator :: Test -> Generator
getGenerator t = fromJust $ find ((`isPrefixOf` testFunction t) . genPrefix) generators

getGenerators :: [Test] -> [Generator]
getGenerators = map head . groupBy  ((==) `on` genPrefix) . sortOn genPrefix . map getGenerator

showImports :: [Test] -> ShowS
showImports = foldEndo . map (\m -> str "import qualified " . str m . nl) . nub . map testModule

showSetup :: Test -> ShowS -> ShowS
showSetup t var = str "  " . var . str " <- " . genSetup (getGenerator t) t . nl

foldEndo :: (Functor f, Foldable f) => f (a -> a) -> (a -> a)
foldEndo = appEndo . fold . fmap Endo

showTestDriver :: FilePath -> [Test] -> ShowS
showTestDriver src ts = let gs = getGenerators ts; vars = map (str . ("t"++) . show) [(0::Int)..] in
    str "{-# LINE 1 " . shows src . str " #-}\n"
  . str "{-# LANGUAGE FlexibleInstances #-}\n"
  . str "module Main where\n"
  . str "import Prelude\n"
  . str "import qualified Test.Tasty as T\n"
  . foldEndo (map genImport gs)
  . showImports ts
  . foldEndo (map genClass gs)
  . str "main :: IO ()\n"
  . str "main = do\n"
  . foldEndo (zipWith showSetup ts vars)
  . str "  T.defaultMain $ T.testGroup " . shows src . str " ["
  . foldEndo (intersperse (',':) $ zipWith (curry snd) ts vars)
  . str "]\n"

filesBySuffix :: FilePath -> [String] -> IO [FilePath]
filesBySuffix dir suffixes = do
  entries <- filter (\s -> head s /= '.') <$> getDirectoryContents dir
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
