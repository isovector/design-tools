{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Snippets
import Text.Pandoc


expandAnnotations :: Inline -> Inline
expandAnnotations = \case
  Link _ [Str t] ("Ann", _) ->
    RawInline (Format "latex") $ "\\ann{" ++ t ++ "}"
  t -> t



inlineCode :: Block -> IO Block
inlineCode = \case
  Para [Link _ [Str t] ("Snip", _)]  -> runSnippet t
  Plain [Link _ [Str t] ("Snip", _)] -> runSnippet t
  t -> pure t

  where
    runSnippet :: String -> IO Block
    runSnippet args = do
      [fp, defn] <- pure $ splitArgs args
      snippet fp defn

    snippet :: FilePath -> String -> IO Block
    snippet fp defn = do
      file <- readFile fp
      pure $ codeBlock $ getDefinition file $ Just defn





codeBlock :: String -> Block
codeBlock = CodeBlock ("", ["haskell"], [])


splitArgs :: String -> [String]
splitArgs s =
  case break (== ':') s of
    ("", "") -> []
    (as, "") -> [as]
    (as, drop 1 -> bs) -> as : splitArgs bs

