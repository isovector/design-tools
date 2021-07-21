{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Ghci (emitGhci, citeLaw) where

import           Cache
import           Combinators
import           Control.Lens
import           Data.Bool
import           Data.Char
import           Data.IORef
import           Data.List
import           Data.Maybe (fromMaybe)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO.Unsafe
import           System.Process
import           Text.Pandoc (readerExtensions, pandocExtensions, def, readMarkdown)
import           Text.Pandoc.Class (runIOorExplode)
import           Text.Pandoc.JSON


ref_laws :: IORef (M.Map Text (M.Map Text Int))
ref_laws = unsafePerformIO $ newIORef mempty
{-# NOINLINE ref_laws #-}


unencodeHtml :: Text -> Text
unencodeHtml
  = T.replace "%20" " "
  . T.replace "%3C" "<"
  . T.replace "%3E" ">"
  . T.replace "%7C" "|"


lookupLawRef :: Text -> Text -> IO Int
lookupLawRef xid (unencodeHtml -> law) = do
  laws <- readIORef ref_laws
  case M.lookup xid laws of
    Just lawm ->
      case M.lookup law lawm of
        Just n  -> pure n
        Nothing -> error $ T.unpack $ "couldn't find law `" <> law <> "` in xid `" <> xid <> "`"
    Nothing -> error $ T.unpack $ "couldn't find xid `" <> xid <> "`"



citeLaw :: Inline -> IO Inline
citeLaw = \case
  Link _ [Str (T.stripPrefix "CiteLaw:" -> Just xid)] (law, _) -> do
    law_id <- lookupLawRef xid law
    pure $ Str $ T.pack $ show law_id
  t -> pure t


doReplace :: Maybe Text -> String -> String
doReplace = appEndo . maybe mempty parseReplace

parseReplace :: Text -> Endo String
parseReplace x =
  flip foldMap (T.split (== '|') x) $ \t ->
    case T.split (== '=') t of
      [lhs, rhs] -> Endo $ T.unpack . T.replace lhs rhs . T.pack
      _ -> error $ "invalid replace string: " <> T.unpack t

emitGhci :: Block -> IO Block
emitGhci (CodeBlock (_, _, kvs) str)
  | Just file <- lookup "ghci" kvs
  = caching (file, str, kvs) $ ghciToPandoc kvs (T.unpack file) (T.unpack str)
emitGhci (CodeBlock attr@(_, _, kvs) str)
  | Just file <- lookup "design" kvs
  = caching (file, str, attr, kvs) $ do
      let fn = fromMaybe "__design" $ lookup "fn" kvs
      designHashToPandoc
        (T.unpack fn)
        kvs
        attr
        (T.unpack file) (
        T.unpack str)
emitGhci (CodeBlock ("", _, kvs) str)
  | Just file <- lookup "quickspec" kvs
  = caching (file, str, kvs) $ fmap snd $ quickspecToPandoc kvs (T.unpack file) (T.unpack str)
emitGhci (CodeBlock (xid, _, kvs) str)
  | Just file <- lookup "quickspec" kvs
  = do
      (mmap, block)
        <- caching (file, str, kvs)
         $ quickspecToPandoc kvs (T.unpack file) (T.unpack str)
      modifyIORef ref_laws $ M.insert xid mmap
      pure block
emitGhci x = pure x


------------------------------------------------------------------------------
-- | Run a function defined in the module, parsing its output as markdown
designHashToPandoc
    :: String -> [(Text, Text)] -> Attr -> FilePath -> String -> IO Block
designHashToPandoc fn kvs attr fp txt = do
  let hash = hashFile (fp, txt, attr)
  rs <- runGhci kvs id fp
      $ unwords
          [ fn
          , show attr
          , show txt
          , show hash
          , "$"
          , txt
          ]
  case rs of
    [(_, r)] -> do
      Pandoc _ p
        <- runIOorExplode
         $ readMarkdown def { readerExtensions = pandocExtensions }
         $ T.pack r
      pure $ Div mempty p
    x -> error $ show x


ghciToPandoc :: [(Text, Text)] -> FilePath -> String -> IO Block
ghciToPandoc kvs fp = fmap format . runGhci kvs id fp

quickspecToPandoc :: [(Text, Text)] -> FilePath -> String -> IO (M.Map Text Int, Block)
quickspecToPandoc kvs fp s = do
  info@[(_, laws)]
    <- runGhci kvs (drop 1 . dropWhile (not . isPrefixOf "== Laws ==")) fp s
  pure (parseLaws laws, format info)


parseLaws :: String -> Map Text Int
parseLaws
  = M.fromList
  . fmap parseLaw
  . filter (not . null)
  . fmap (dropWhile isSpace)
  . lines
  where
    -- "parses" things of the form /\s*([0-9]+)\. (.*)/ as (\2, \1)
    parseLaw :: String -> (Text, Int)
    parseLaw s =
      let (num_s, s') = span isDigit s
          num         = read num_s
          s''         = drop 2 s'
       in (T.pack s'', num)


format :: [(String, String)] -> Block
format
  = CodeBlock ("", ["haskell", "ghci"], [])
  . T.pack
  . unlines
  . fmap (\(req, resp) -> unlines ["> " ++ req, resp])


runGhci :: [(Text, Text)] -> ([String] -> [String]) -> FilePath -> String -> IO [(String, String)]
runGhci kvs f fp str
  = fmap (zip (lines $ replace str) . responses f . replace)
  . readProcess "stack" ["repl", "--no-load"]
  $ unlines [":l " ++ fp, str]
 where replace = doReplace $ lookup "replace" kvs


responses :: ([String] -> [String]) -> String -> [String]
responses f
  = fmap unlines
  . fmap f
  . fmap (_head %~ removeManyTags)
  . groupBy (\_ a -> not $ isResponse a)
  . drop 1
  . dropWhile (not . isPrefixOf "Ok, ")
  . lines


removeTag :: String -> String
removeTag = drop 2 . dropWhile (/= '>')


removeManyTags :: String -> String
removeManyTags ts = bool ts (removeManyTags $ removeTag ts) $ isResponse ts


isResponse :: String -> Bool
isResponse ('*':_) = True
isResponse _ = False

