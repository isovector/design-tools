{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Control.Monad
import Snippets
import Text.Pandoc


expandAnnotations :: Inline -> Inline
expandAnnotations = \case
  Link _ [Str t] ("Ann", _) ->
    RawInline (Format "latex") $ "\\ann{" ++ t ++ "}"
  t -> t



inlineSnippets :: Block -> IO Block
inlineSnippets = \case
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


exercises :: Block -> Block
exercises = \case
  DefinitionList [([Str "Exercise"], bs)] ->
    Div ("", [], []) $ join
      [ pure $ Para [Str ""]
      , pure . Plain . pure $ RawInline (Format "latex") "\\begin{exercise}"
      , join bs
      , pure . Plain . pure $ RawInline (Format "latex") "\\end{exercise}"
      , pure $ Para [Str ""]
      ]
  t -> t





codeBlock :: String -> Block
codeBlock = CodeBlock ("", ["haskell"], [])


splitArgs :: String -> [String]
splitArgs s =
  case break (== ':') s of
    ("", "") -> []
    (as, "") -> [as]
    (as, drop 1 -> bs) -> as : splitArgs bs

