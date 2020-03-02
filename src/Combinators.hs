{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Combinators where

import Control.Monad
import Text.Pandoc
import Data.List

codeToLatexCmd :: (String -> Maybe String) -> String -> Inline -> Inline
codeToLatexCmd match cmd = \case
  Code _ (match -> Just t) ->
    RawInline (Format "latex") $ "\\" ++ cmd ++ "{" ++ t ++ "}"
  t -> t

prefixCodeToLatexCmd :: String -> String -> Inline -> Inline
prefixCodeToLatexCmd prefix = codeToLatexCmd (stripPrefix prefix)


linkToLatexCmd :: String -> String -> Inline -> Inline
linkToLatexCmd match with = \case
  Link _ [Str t] (name, _)
    | match == name ->
        RawInline (Format "latex") $ "\\" ++ with ++ "{" ++ t ++ "}"
  t -> t

defnToLatexEnv :: String -> String -> Block -> Block
defnToLatexEnv match with = \case
  DefinitionList [([Str name], bs)]
    | name == match ->
        mkEnv with $ join bs
  t -> t


quoteToDefn :: String -> String -> Block -> Block
quoteToDefn match with = \case
  BlockQuote (Para (Str lead : ps) : bs)
    | lead == match ->
        DefinitionList [([Str with], [Para ps : bs])]
  t -> t


mkEnv :: String -> [Block] -> Block
mkEnv env bs =
  Div ("", [], []) $ join
    [ pure $ Para [Str ""]
    , pure . Plain . pure . RawInline (Format "latex") $ "\\begin{" ++ env ++ "}"
    , bs
    , pure . Plain . pure . RawInline (Format "latex") $ "\\end{" ++ env ++ "}"
    , pure $ Para [Str ""]
    ]

