{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Lib where

import           CSV
import           Combinators
import           Control.Arrow
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable
import           Data.List
import           Data.List (uncons, sort)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Snippets
import           Text.Pandoc


wrapCodeEnv :: Format -> Text -> Text -> Block -> Block
wrapCodeEnv format cls div (CodeBlock (ident, attrs, kvs) code)
  | elem cls attrs
      = mkEnv format div
      $ pure
      $ CodeBlock (ident, filter (/= cls) attrs, kvs) code
wrapCodeEnv _ _ _ t = t


inlineSnippets :: Block -> IO Block
inlineSnippets = \case
  Para [Link _ (Strs t) ("Snip", _)]  -> runSnippet t
  Plain [Link _ (Strs t) ("Snip", _)] -> runSnippet t
  t -> pure t

  where
    runSnippet :: Text -> IO Block
    runSnippet args = do
      (fp : more_args) <- pure $ splitArgs args
      snippet (T.unpack fp) $ fmap fst $ uncons more_args

    snippet :: FilePath -> Maybe Text -> IO Block
    snippet fp defn = do
      file <- readFile fp
      pure $ codeBlock $ T.pack $ getDefinition fp file $ fmap T.unpack defn


showCSV :: Block -> IO Block
showCSV = \case
  Para [Link _ (Strs t) ("CSV", _)]  -> runCSV t
  Plain [Link _ (Strs t) ("CSV", _)] -> runCSV t
  t -> pure t

  where
    runCSV :: Text -> IO Block
    runCSV args = do
      (fp : more_args) <- pure $ splitArgs args
      csv <- loadCSV <$> BS.readFile (T.unpack fp)
      pure $ case more_args of
        [proj] -> showVector $ selectCSV proj csv
        [field, value, proj] ->
          showVector $ selectCSV proj $ filterCSV field value csv
        _ -> error $ "bad argument format given to CSV: " <> T.unpack args


pattern Strs :: Text -> [Inline]
pattern Strs ts <-
  ((id &&& id)
    ->
      ( all isStr -> True
      , foldMap fromStr -> ts
      )
  )

isStr :: Inline -> Bool
isStr (Str _) = True
isStr Space = True
isStr _ = False

fromStr :: Inline -> Text
fromStr (Str s) = s
fromStr Space = " "


showVector :: V.Vector Text -> Block
showVector = BulletList
           . fmap (pure . Plain . pure . Str)
           . sort
           . toList


codeBlock :: Text -> Block
codeBlock = CodeBlock ("", ["haskell"], [])


splitArgs :: Text -> [Text]
splitArgs s =
  case T.break (== ':') s of
    ("", "") -> []
    (as, "") -> [as]
    (as, T.drop 1 -> bs) -> as : splitArgs bs

