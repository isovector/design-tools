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

module AgdaDump (doHighlight) where

import           Cache
import           Control.Lens (set, over)
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Writer.CPS (runWriterT)
import           Control.Monad.Writer.CPS
import           Data.Char (isSpace)
import           Data.Generics.Product.Fields
import           Data.Hashable
import           Data.List (sort)
import           Data.List.Extra
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Generics
import           System.FilePath
import           System.Directory
import           System.IO
import           System.Process
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Text.Read (readMaybe)
import System.Exit


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
  = Illegal
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
  , st_next :: Int
  }
  deriving (Generic)


nextInlineName :: ExtractM Text
nextInlineName
  = fmap (mappend "inline-" . T.pack . show)
  $ gets st_next <* modify (over (field @"st_next") (+ 1))


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


emitRealCode :: Text -> ExtractM ()
emitRealCode t = do
    tell [t]
    let ls = filter ((/= "") . T.strip) $ T.lines t
    when (not $ null ls) $ do
      modify $ set (field @"st_indent") $ T.length $ T.takeWhile isSpace $ last $ ls

------------------------------------------------------------------------------
-- | Extract literate Agda code from the file.
getRealAndInlineCode :: Block -> ExtractM Block
getRealAndInlineCode c@(CodeBlock (_, cls, _) t)
  | not $ elem "agda" cls = pure c
  | otherwise = do
      emitRealCode t
      pure c
getRealAndInlineCode c = walkM getInlineCode c

------------------------------------------------------------------------------
-- | Extract literate Agda code from the file.
getRealAndIllegalCode :: Block -> ExtractM Block
getRealAndIllegalCode c@(CodeBlock (_, cls, _) t)
  | elem "agda" cls = do
      emitRealCode t
      pure c
getRealAndIllegalCode c = getIllegalCode c


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
  lift $ lift $ tell [key]
  tell . pure $ mkBanner "START" key
  r <- m
  tell . pure $ mkBanner "END" key
  pure r


------------------------------------------------------------------------------
-- | Pattern synonym for "extending" the pandoc AST with references to Illegal
-- Agda snippets
pattern IllegalRef :: DumpKey -> Block
pattern IllegalRef key <- CodeBlock _ (readMaybe . T.unpack -> Just key)
  where
    IllegalRef = CodeBlock ("", [], []) . T.pack . show

------------------------------------------------------------------------------
-- | Pattern synonym for "extending" the pandoc AST with references to inline
-- Agda snippets
pattern InlineRef :: DumpKey -> Inline
pattern InlineRef key <- Code _ (readMaybe . T.unpack -> Just key)
  where
    InlineRef = Code ("", [], []) . T.pack . show


------------------------------------------------------------------------------
-- | Replace an invalid code block with an IllegalRef, emitting it into the
-- code we're going to scope-check.
getIllegalCode :: Block -> ExtractM Block
getIllegalCode c@(CodeBlock (_, cls, _) t)
  | not $ elem "illegal" cls = pure c
  | otherwise = do
      let key = DumpKey Illegal $ hash $ show c
      extractMe key $ tell [t]
      pure $ IllegalRef key
getIllegalCode c = pure c

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
  | Just tb <- T.stripPrefix "bind:" t
  , (b, T.drop 1 -> t') <- T.span (/= ':') tb
  = do
      indent <- gets st_indent
      name <- nextInlineName
      let key = DumpKey Inline $ hash $ show c
      let i = T.replicate indent " "
      tell [i <> name <> " : _"]
      extractMe key $ tell [i <> name <> " " <> b <> " = " <> t']
      pure $ InlineRef key
getInlineCode c = pure c


workingDir :: FilePath
workingDir = "/home/sandy/prj/math-for-programmers/build/tex/agda"

------------------------------------------------------------------------------
-- | Given a module name, run the 'ExtractM' monad, dropping the resulting
-- emitted code into @/tmp@
runExtract :: Text -> ExtractM a -> IO ([DumpKey], a)
runExtract modul m = do
  let ((a, w), keys) = runWriter $ runWriterT $ flip evalStateT (St 0 0) m
      modul' = modul <> "-dump"
      out = T.replace modul modul' $ T.unlines w
      keyset = sort keys
  T.writeFile (workingDir </> T.unpack modul' <> ".lagda.tex") out
  pure (keyset, a)


------------------------------------------------------------------------------
-- | Worker function that extracts all inline code from a literate agda
-- document, emitting it to a seperate file, running the syntax highlighter on
-- that, and then splices the formatted syntax back into the original pandoc
-- document.
doHighlight :: Pandoc -> IO Pandoc
doHighlight p = do
  let modul = fromMaybe "" $ getFirst $ query getModule p
      modul' = modul <> "-dump"

  case modul == "" of
    True -> pure p
    False -> do
      -- Get the dumpkey set from the document, and emit the necessary code to
      -- check.
      (inl_keyset, p') <- runExtract modul $ do
        tell ["\\begin{code}"]
        p' <- walkM getRealAndInlineCode p
        tell ["\\end{code}"]
        pure p'

      -- Run @agda --latex@ on the emitted code, and then parse out the highlighted
      -- code.
      --
      -- 'caching' uses the keyset as a cache key, meaning we don't need to rerun
      -- this step if the actual bits to highlight haven't changed since last time.
      -- The keyset itself depends on the hash of each snippet.
      inl_dump <- caching (Inline, inl_keyset, Version 0) $ do
        (code, out, _)
          <- withCurrentDirectory workingDir
            $ readProcessWithExitCode "agda" ["--latex", "--only-scope-checking", T.unpack modul' <> ".lagda.tex"] ""
        unless (code == ExitSuccess) $ do
          hPutStr stderr out
          error "agda died"

        f <- T.readFile $ workingDir </> "latex" </> T.unpack modul' <> ".tex"
        pure $ parseHighlightedAgda f
      hPrint stderr inl_keyset


      -- Now do it all again, but this time for invalid code
      -- check.
      (inv_keyset, p'') <- runExtract modul $ do
        tell ["\\begin{code}"]
        p'' <- walkM getRealAndIllegalCode p'
        tell ["\\end{code}"]
        pure p''
      hPrint stderr inv_keyset
      inv_dump <- caching (Illegal, inv_keyset, Version 0) $ do
        (code, out, _)
          <- withCurrentDirectory workingDir
            $ readProcessWithExitCode "agda" ["--latex", "--only-scope-checking", T.unpack modul' <> ".lagda.tex"] ""

        unless (code == ExitSuccess) $ do
          hPutStr stderr out
          error "agda died"
        f <- T.readFile $ workingDir </> "latex" </> T.unpack modul' <> ".tex"
        pure $ parseHighlightedAgda f

      let dump = inl_dump <> inv_dump
      pure $ walk (spliceInline dump) $ walk (spliceBlock dump) p''


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
-- 'IllegalRef' with the highlighted results.
spliceBlock :: Map DumpKey Text -> Block -> Block
spliceBlock dk (IllegalRef key)
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
        DumpKey Illegal _ -> "\\begin{VERYILLEGALCODE}[indent=666]%\n%\n" <> t <> "\\end{VERYILLEGALCODE}"
    )
  . fmap T.unlines
  . M.mapWithKey (\case
      DumpKey Inline _ ->
        fmap ( T.replace "\\AgdaSpace{}" "~"
             . T.replace "\\<" ""
             )
         . init
         . drop 1
         . dropWhile (not . T.isInfixOf "\\AgdaSymbol{=}")
      DumpKey Illegal _ -> dropEnd 1 . drop 1
                 )
  . extractBetweenBanners
  . T.lines

