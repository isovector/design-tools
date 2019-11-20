{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Data.Foldable
import Data.List (uncons)
import Snippets
import Text.Pandoc
import CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V


inlineSnippets :: Block -> IO Block
inlineSnippets = \case
  Para [Link _ [Str t] ("Snip", _)]  -> runSnippet t
  Plain [Link _ [Str t] ("Snip", _)] -> runSnippet t
  t -> pure t

  where
    runSnippet :: String -> IO Block
    runSnippet args = do
      (fp : more_args) <- pure $ splitArgs args
      snippet fp $ fmap fst $ uncons more_args

    snippet :: FilePath -> Maybe String -> IO Block
    snippet fp defn = do
      file <- readFile fp
      pure $ codeBlock $ getDefinition fp file defn


showCSV :: Block -> IO Block
showCSV = \case
  Para [Link _ [Str t] ("CSV", _)]  -> runCSV t
  Plain [Link _ [Str t] ("CSV", _)] -> runCSV t
  t -> pure t

  where
    runCSV :: String -> IO Block
    runCSV args = do
      (fp : more_args) <- pure $ splitArgs args
      csv <- loadCSV <$> BS.readFile fp
      pure $ case more_args of
        [proj] -> showVector $ selectCSV proj csv
        [field, value, proj] ->
          showVector $ selectCSV proj $ filterCSV field value csv
        _ -> error $ "bad argument format given to CSV: " <> args


showVector :: V.Vector String -> Block
showVector = BulletList
           . fmap (pure . Plain . pure . Str)
           . toList


codeBlock :: String -> Block
codeBlock = CodeBlock ("", ["haskell"], [])


splitArgs :: String -> [String]
splitArgs s =
  case break (== ':') s of
    ("", "") -> []
    (as, "") -> [as]
    (as, drop 1 -> bs) -> as : splitArgs bs

