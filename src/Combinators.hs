{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Combinators where

import Control.Monad
import Text.Pandoc
import Data.List

codeToLatexCmd :: Format -> (String -> Maybe String) -> String -> Inline -> Inline
codeToLatexCmd format match cmd = \case
  Code _ (match -> Just t) -> mkInline format cmd t
  t -> t

prefixCodeToLatexCmd :: Format -> String -> String -> Inline -> Inline
prefixCodeToLatexCmd format prefix = codeToLatexCmd format (stripPrefix prefix)


linkToLatexCmd :: Format -> String -> String -> Inline -> Inline
linkToLatexCmd format match with = \case
  Link _ [Str t] (name, _)
    | match == name -> mkInline format with t
  t -> t

mkInline :: Format -> String -> String -> Inline
mkInline (Format "epub") cls content = Span ("", [cls], []) [Str content]
mkInline (Format "latex") cls content =
  RawInline (Format "latex") $ "\\" ++ cls ++ "{" ++ content ++ "}"

defnToLatexEnv :: Format -> String -> String -> Block -> Block
defnToLatexEnv format match with = \case
  DefinitionList [([Str name], bs)]
    | name == match ->
        mkEnv format with $ join bs
  t -> t


quoteToDefn :: String -> String -> Block -> Block
quoteToDefn match with = \case
  BlockQuote (Para (Str lead : ps) : bs)
    | lead == match ->
        DefinitionList [([Str with], [Para ps : bs])]
  t -> t


mkEnv :: Format -> String -> [Block] -> Block
mkEnv (Format "latex") env bs =
  Div ("", [], []) $ join
    [ pure $ Para [Str ""]
    , pure . Plain . pure . RawInline (Format "latex") $ "\\begin{" ++ env ++ "}"
    , bs
    , pure . Plain . pure . RawInline (Format "latex") $ "\\end{" ++ env ++ "}"
    , pure $ Para [Str ""]
    ]
mkEnv (Format "epub") env bs = Div ("", [env], []) bs

