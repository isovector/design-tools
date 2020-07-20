{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Combinators where

import Control.Monad
import Text.Pandoc
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

codeToLatexCmd :: Format -> (Text -> Maybe Text) -> Text -> Inline -> Inline
codeToLatexCmd format match cmd = \case
  Code _ (match -> Just t) -> mkInline format cmd t
  t -> t

prefixCodeToLatexCmd :: Format -> Text -> Text -> Inline -> Inline
prefixCodeToLatexCmd format prefix = codeToLatexCmd format (T.stripPrefix prefix)


linkToLatexCmd :: Format -> Text -> Text -> Inline -> Inline
linkToLatexCmd format match with = \case
  Link _ [Str t] (name, _)
    | match == name -> mkInline format with t
  t -> t

mkInline :: Format -> Text -> Text -> Inline
mkInline (Format "epub") cls content = Span ("", [cls], []) [Str content]
mkInline (Format "latex") cls content =
  RawInline (Format "latex") $ "\\" <> cls <> "{" <> content <> "}"

defnToLatexEnv :: Format -> Text -> Text -> Block -> Block
defnToLatexEnv format match with = \case
  DefinitionList [([Str name], bs)]
    | name == match ->
        mkEnv format with [] $ join bs
  t -> t


quoteToDefn :: Text -> Text -> Block -> Block
quoteToDefn match with = \case
  BlockQuote (Para (Str lead : ps) : bs)
    | lead == match ->
        DefinitionList [([Str with], [Para ps : bs])]
  t -> t


mkEnv :: Format -> Text -> [Text] -> [Block] -> Block
mkEnv (Format "latex") env args bs =
  Div ("", [], []) $ join
    [ pure $ Para [Str ""]
    , pure . Plain . pure . RawInline (Format "latex") $ "\\begin{" <> env <> "}" <>
        case args of
          [] -> ""
          _ -> "{" <> T.intercalate "}{" args <> "}"
    , bs
    , pure . Plain . pure . RawInline (Format "latex") $ "\\end{" <> env <> "}"
    , pure $ Para [Str ""]
    ]
mkEnv (Format "epub") env _ bs = Div ("", [env], []) bs

