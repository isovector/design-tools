{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Data.List (uncons)
import Snippets
import Text.Pandoc


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


codeBlock :: String -> Block
codeBlock = CodeBlock ("", ["haskell"], [])


splitArgs :: String -> [String]
splitArgs s =
  case break (== ':') s of
    ("", "") -> []
    (as, "") -> [as]
    (as, drop 1 -> bs) -> as : splitArgs bs

