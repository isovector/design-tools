{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Combinators where

import           Control.Monad
import           Data.Either (fromLeft, fromRight, isRight)
import           Data.Foldable (asum)
import           Data.Function
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Debug.Trace
import           Text.Pandoc
import           Utils

codeToLatexCmd :: Format -> (Text -> Maybe Text) -> Text -> Inline -> Inline
codeToLatexCmd format match cmd = \case
  Code _ (match -> Just t) -> mkInline format cmd t
  t -> t

br :: Format -> Text -> Inline -> Inline
br format match = \case
  Code _ ((== match) -> True) ->
    case format of
      Format "latex" -> RawInline format "\\\\"
      Format "epub"  -> RawInline format "<br/>"
      _              -> error "unknown format when producing <br>"
  t -> t

prefixCodeToLatexCmd :: Format -> Text -> Text -> Inline -> Inline
prefixCodeToLatexCmd format prefix = codeToLatexCmd format (T.stripPrefix prefix)

linkToLatexCmd :: Format -> Text -> Text -> Inline -> Inline
linkToLatexCmd format match with = \case
  Link _ (Strs t) (name, _)
    | match == name -> mkInline format with t
  t -> t

escapeLatex :: Text -> Text
escapeLatex
  = T.replace "~" "\\sim{}"
  . T.replace "$" "\\$"
  . T.replace "_" "\\textunderscore{}"
  . T.replace "^" "\\textasciicircum{}"

mkInline :: Format -> Text -> Text -> Inline
mkInline (Format "epub") cls content = Span ("", [cls], []) [Str content]
mkInline (Format "latex") cls content =
  RawInline (Format "latex") $ "\\" <> cls <> "{" <> escapeLatex content <> "}"

defnToLatexEnv :: Format -> Text -> Text -> Block -> Block
defnToLatexEnv format match with = \case
  t@(DefinitionList defs) -> do
    let matching
          = fmap (\case
                    (Strs name, bs) | name == match -> Right bs
                    t -> Left t
                 ) defs
    case any isRight matching of
      False -> t
      True -> Div mempty $ do
        g <- groupBy (on (==) isRight) matching
        case head g of
          Left _ -> pure $ DefinitionList $ fmap (fromLeft undefined) g
          Right _ -> do
            Right e <- g
            pure $ mkEnv format with [] $ join e
  t -> t

defnOnlyInFormat :: Format -> Text -> Text -> Block -> Block
defnOnlyInFormat (Format format) only match = \case
  DefinitionList [(Strs (traceShowId -> name), bs)]
    | name == match ->
        case format == only of
          True -> Div ("", [], []) $ join bs
          False -> Plain []
  t -> t

-- codeBlockToEnv :: Format -> Text -> Text -> Block -> Block
-- defnToLatexEnv format match with = \case
--   DefinitionList [([Str name], bs)]
--     | name == match ->
--         mkEnv format with [] $ join bs
--   t -> t


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
mkEnv (Format "epub") env args bs =
  Div ("", [env], zipWith (\ix arg -> ("data-arg" <> T.pack (show ix), arg)) [1..] args) bs

mkRawEnv :: Format -> Text -> [Text] -> Text -> Block
mkRawEnv (Format "latex") env args bs =
  Div ("", [], []) $ join
    [ pure . Plain . pure . RawInline (Format "latex") $
      mconcat
        [ "\\begin{" <> env <> "}"
        , case args of
            [] -> ""
            _ -> "{" <> T.intercalate "}{" args <> "}"
        , "\n"
        , bs
        , "\n"
        , "\\end{" <> env <> "}"
        ]
    , pure $ Para [Str ""]
    ]
mkRawEnv (Format "epub") env args bs =
  Div ("", [env], zipWith (\ix arg -> ("data-arg" <> T.pack (show ix), arg)) [1..] args) [ Plain [Str bs] ]

