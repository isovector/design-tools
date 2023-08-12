{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# OPTIONS_GHC -Wall          #-}

module AgdaDump (highlightInline) where

import Cache
import           Control.Lens (set)
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Writer.CPS (runWriterT)
import           Control.Monad.Writer.CPS
import           Data.Char (isSpace)
import           Data.Generics.Product.Fields
import           Data.Hashable
import           Data.List (sort)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Generics
import           System.Directory
import           System.Process
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Text.Read


------------------------------------------------------------------------------
-- | Wrapper for versioning the cached results while developing this tool
newtype Version = Version Int
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Hashable Version


------------------------------------------------------------------------------
-- | This module can be used to highlight broken agda code (via -- --only-scope-checking),
-- as well as inline blocks of code. 'DumpSort' lets us differentiate between
-- the two cases, since we need to do different processing on them.
data DumpSort
  = Invalid
  | Inline
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

------------------------------------------------------------------------------
-- | A key used to keep track of individual snippets
data DumpKey = DumpKey DumpSort Int
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable DumpSort
instance Hashable DumpKey


------------------------------------------------------------------------------
-- | State for the extraction monad.
data St = St
  { st_indent :: Int  -- ^ The current indentation level
  }
  deriving (Generic)


------------------------------------------------------------------------------
-- | The main monad for doing snippet highlighting. We keep track of state,
-- have a writer for code we need to emit, and another writer for 'DumpKey's
-- that we synthesize as we go.
type ExtractM = StateT St (WriterT [Text] (Writer [DumpKey]))


------------------------------------------------------------------------------
-- | Extract the module name of the current literate agda file.
getModule :: Block -> First Text
getModule (CodeBlock (_, cls, _) t)
  | elem "agda" cls = flip foldMap (T.lines t) $ \l ->
      case T.stripPrefix "module" l of
        Just m -> pure $ head $ T.words m
        Nothing -> mempty
getModule _ = mempty


------------------------------------------------------------------------------
-- | Extract literate Agda code from the file.
getRealCode :: Block -> ExtractM Block
getRealCode c@(CodeBlock (_, cls, _) t)
  | not $ elem "agda" cls = pure c
  | otherwise = do
      tell [t]
      let ls = filter ((/= "") . T.strip) $ T.lines t
      when (not $ null ls) $ do
        modify $ set (field @"st_indent") $ T.length $ T.takeWhile isSpace $ last $ ls
      pure c
getRealCode c = walkM getInlineCode c


------------------------------------------------------------------------------
-- | Generate a comment banner for the generated code, that we can look up
-- later and extract highlighted results from.
mkBanner :: Text -> DumpKey -> Text
mkBanner z key = mconcat
  [ "-- >>>>>>>>>> "
  , z
  , " "
  , T.pack $ show key
  ]


------------------------------------------------------------------------------
-- | Wrap the given monadic action in a START and END banner.
extractMe :: DumpKey -> ExtractM a -> ExtractM a
extractMe key m = do
  tell . pure $ mkBanner "START" key
  r <- m
  tell . pure $ mkBanner "END" key
  pure r


------------------------------------------------------------------------------
-- | Pattern synonym for "extending" the pandoc AST with references to Invalid
-- Agda snippets
pattern InvalidRef :: DumpKey -> Block
pattern InvalidRef key <- CodeBlock _ (readMaybe . T.unpack -> Just key)
  where
    InvalidRef = CodeBlock ("", [], []) . T.pack . show

------------------------------------------------------------------------------
-- | Pattern synonym for "extending" the pandoc AST with references to inline
-- Agda snippets
pattern InlineRef :: DumpKey -> Inline
pattern InlineRef key <- Code _ (readMaybe . T.unpack -> Just key)
  where
    InlineRef = Code ("", [], []) . T.pack . show


------------------------------------------------------------------------------
-- | Replace an invalid code block with an InvalidRef, emitting it into the
-- code we're going to scope-check.
getInvalidCode :: Block -> ExtractM Block
getInvalidCode c@(CodeBlock (_, cls, _) t)
  | not $ elem "invalid" cls = pure c
  | otherwise = do
      let key = DumpKey Invalid $ hash $ show c
      extractMe key $ tell [t]
      pure $ InvalidRef key
getInvalidCode c = pure c

------------------------------------------------------------------------------
-- | Replace a inline code block with an InlineRef, emitting it and a @_ =@
-- header into the code we're going to highlight.
getInlineCode :: Inline -> ExtractM Inline
getInlineCode c@(Code _ t)
  | Just t' <- T.stripPrefix "expr:" t =  do
      indent <- gets st_indent
      let key = DumpKey Inline $ hash $ show c
      extractMe key $ tell [T.replicate indent " " <> "_ = " <> t']
      pure $ InlineRef key
getInlineCode c = pure c


------------------------------------------------------------------------------
-- | Given a module name, run the 'ExtractM' monad, dropping the resulting
-- emitted code into @/tmp@
runExtract :: Text -> ExtractM a -> IO ([DumpKey], a)
runExtract modul m = do
  let ((a, w), keys) = runWriter $ runWriterT $ flip evalStateT (St 0) m
      modul' = modul <> "-dump"
      out = T.replace modul modul' $ T.unlines w
      keyset = sort keys
  T.writeFile ("/tmp/" <> T.unpack modul' <> ".lagda.tex") out
  pure (keyset, a)


------------------------------------------------------------------------------
-- | Worker function that extracts all inline code from a literate agda
-- document, emitting it to a seperate file, running the syntax highlighter on
-- that, and then splices the formatted syntax back into the original pandoc
-- document.
highlightInline :: Pandoc -> IO Pandoc
highlightInline p = do
  let First (Just modul) = query getModule p
      modul' = modul <> "-dump"

  -- Get the dumpkey set from the document, and emit the necessary code to
  -- check.
  (keyset, p') <- runExtract modul $ do
    tell ["\\begin{code}"]
    p' <- walkM getRealCode p
    tell ["\\end{code}"]
    pure p'

  -- Run @agda --latex@ on the emitted code, and then parse out the highlighted
  -- code.
  --
  -- 'caching' uses the keyset as a cache key, meaning we don't need to rerun
  -- this step if the actual bits to highlight haven't changed since last time.
  -- The keyset itself depends on the hash of each snippet.
  dump <- caching (keyset, Version 0) $ do
    _ <- withCurrentDirectory "/tmp"
       $ readProcess "agda" ["--latex", T.unpack modul' <> ".lagda.tex"] ""
    f <- T.readFile $ "/tmp/latex/" <> T.unpack modul' <> ".tex"
    pure $ parseHighlightedAgda f

  pure $ walk (spliceInline dump) $ walk (spliceBlock dump) p'


------------------------------------------------------------------------------
-- | Given the lines of a source file, find the bits we'd like to extract
-- (which we have set up to be between banners generated by 'mkBanner'.)
extractBetweenBanners :: [Text] -> Map DumpKey [Text]
extractBetweenBanners = go undefined [[]]
  where
    isStart :: Text -> Bool
    isStart = T.isInfixOf ">>>>>>>>>>\\ START"

    isEnd :: Text -> Bool
    isEnd = T.isInfixOf ">>>>>>>>>>\\ END"

    parseDumpKey :: Text -> DumpKey
    parseDumpKey
      = maybe (error "no key") (read . T.unpack)
      . T.stripPrefix "-- >>>>>>>>>> START "
      . T.drop 18
      . T.dropEnd 4
      . T.replace "\\ " " "

    go :: DumpKey -> [[Text]] -> [Text] -> Map DumpKey [Text]
    go key (acc : accs) (t : ts)
      | isStart t = go (parseDumpKey t) ([] : acc : accs) ts
      | isEnd t = M.insert key (reverse acc) $ go undefined accs ts
      | otherwise = go key ((t : acc) : accs) ts
    go _ _ _ = mempty


------------------------------------------------------------------------------
-- | code to test 'extractBetweenBanners'
test :: Text
test = T.unlines
  [ "\\>[0]\\AgdaComment{--\\ >>>>>>>>>>\\ START\\ DumpKey\\ Inline\\ 0}\\<%"
  , "a"
  , "b"
  , "\\>[0]\\AgdaComment{--\\ >>>>>>>>>>\\ END\\ DumpKey\\ Inline\\ 0}\\<%"
  ]


------------------------------------------------------------------------------
-- | Splice a 'DumpKey' map back into a 'Block'; in essence, replacing the
-- 'InvalidRef' with the highlighted results.
spliceBlock :: Map DumpKey Text -> Block -> Block
spliceBlock dk (InvalidRef key)
  | Just t <- M.lookup key dk
  = RawBlock "latex" t
spliceBlock _ b = b

------------------------------------------------------------------------------
-- | Splice a 'DumpKey' map back into a 'Inline'; in essence, replacing the
-- 'InlineRef' with the highlighted results.
spliceInline :: Map DumpKey Text -> Inline -> Inline
spliceInline dk (InlineRef key)
  | Just t <- M.lookup key dk
  = RawInline "latex" t
spliceInline _ b = b


------------------------------------------------------------------------------
-- | Given the source of a highlighted Agda tex document, parse out the DumpKey
-- map corresponding to highlighted results.
parseHighlightedAgda :: Text -> Map DumpKey Text
parseHighlightedAgda
  = M.mapWithKey (\k t ->
      case k of
        DumpKey Inline _ -> T.replace "%\n" "" t
        _ -> t
    )
  . fmap T.unlines
  . fmap ( fmap ( T.replace "\\AgdaSpace{}" "~"
                . T.replace "\\<" ""
                )
         . init
         . drop 4
         )
  . extractBetweenBanners
  . T.lines

