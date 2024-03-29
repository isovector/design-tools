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

import Debug.Trace
import           Cache
import           Control.Lens (set, over)
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Writer.CPS (runWriterT)
import           Control.Monad.Writer.CPS
import           Data.Bool
import           Data.Char (isSpace)
import           Data.Generics.Product.Fields
import           Data.Hashable
import           Data.List.Extra
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Generics
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Text.Read (readMaybe)


------------------------------------------------------------------------------
-- | Wrapper for versioning the cached results while developing this tool
newtype Version = Version Int
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Hashable Version
instance Hashable Format


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


workingDir :: Format -> FilePath
workingDir (Format "latex") = "/home/sandy/prj/math-for-programmers/build/tex/agda"
workingDir (Format "epub") = "/home/sandy/prj/math-for-programmers/build-epub/agda"
workingDir _ = error "bad format"

------------------------------------------------------------------------------
-- | Given a module name, run the 'ExtractM' monad, dropping the resulting
-- emitted code into @/tmp@
runExtract :: Format -> Text -> ExtractM a -> IO ([DumpKey], a)
runExtract format modul m = do
  let ((a, w), keys) = runWriter $ runWriterT $ flip evalStateT (St 0 0) m
      modul' = modul <> "-dump"
      out = T.replace modul modul' $ T.unlines w
      keyset = sort keys
  T.writeFile (workingDir format </> T.unpack modul' <> ".lagda" <.>
    case format of
        Format "latex" -> "tex"
        Format "epub" -> "md"
        _ -> error "bad format"
      ) out
  pure (keyset, a)


------------------------------------------------------------------------------
-- | Worker function that extracts all inline code from a literate agda
-- document, emitting it to a seperate file, running the syntax highlighter on
-- that, and then splices the formatted syntax back into the original pandoc
-- document.
doHighlight :: Format -> Pandoc -> IO Pandoc
doHighlight format p = do
  let modul = fromMaybe "" $ getFirst $ query getModule p
      modul' = modul <> "-dump"

      (startCode, endCode, agdaArgs, outputdir, ext) =
        case format of
          Format "latex" ->
            ( tell ["\\begin{code}"]
            , tell ["\\end{code}"]
            , ["--latex", "--only-scope-checking", T.unpack modul' <> ".lagda.tex"]
            , "latex"
            , "tex"
            )
          Format "epub"  ->
            ( tell ["```agda"]
            , tell ["```"]
            , ["--html", "--html-highlight=code", "--only-scope-checking", T.unpack modul' <> ".lagda.md"]
            , "html"
            , "md"
            )
          _ -> error "bad format"

  case modul == "" of
    True -> pure p
    False -> do
      -- Get the dumpkey set from the document, and emit the necessary code to
      -- check.
      (inl_keyset, p') <- runExtract format modul $ do
        startCode
        p' <- walkM getRealAndInlineCode p
        endCode
        pure p'

      -- Run @agda --latex@ on the emitted code, and then parse out the highlighted
      -- code.
      --
      -- 'caching' uses the keyset as a cache key, meaning we don't need to rerun
      -- this step if the actual bits to highlight haven't changed since last time.
      -- The keyset itself depends on the hash of each snippet.
      inl_dump <- caching (Inline, inl_keyset, format, Version 5) $ do
        (code, out, _)
          <- withCurrentDirectory (workingDir format)
            $ readProcessWithExitCode "agda" agdaArgs ""
        unless (code == ExitSuccess) $ do
          hPutStr stderr out
          error "agda died"

        traceM $  workingDir format </> outputdir </> T.unpack modul' <.> ext
        f <- T.readFile $ workingDir format </> outputdir </> T.unpack modul' <.> ext
        pure $ parseHighlightedAgda format f

      -- Now do it all again, but this time for invalid code
      -- check.
      (inv_keyset, p'') <- runExtract format modul $ do
        startCode
        p'' <- walkM getRealAndIllegalCode p'
        endCode
        pure p''
      hPrint stderr inv_keyset
      inv_dump <- caching (Illegal, inv_keyset, format, Version 5) $ do
        (code, out, _)
          <- withCurrentDirectory (workingDir format)
            $ readProcessWithExitCode "agda" agdaArgs ""

        unless (code == ExitSuccess) $ do
          hPutStr stderr out
          error "agda died"
        f <- T.readFile $ workingDir format </> outputdir </> T.unpack modul' <.> ext
        pure $ parseHighlightedAgda format f

      let dump = inl_dump <> inv_dump
      pure $ walk (spliceInline format dump) $ walk (spliceBlock format dump) p''


------------------------------------------------------------------------------
-- | Given the lines of a source file, find the bits we'd like to extract
-- (which we have set up to be between banners generated by 'mkBanner'.)
extractBetweenBanners :: Format -> [Text] -> Map DumpKey [Text]
extractBetweenBanners format = go undefined [[]]
  where
    isStart :: Text -> Bool
    isStart t = T.isInfixOf ">>>>>>>>>>\\ START" t
             || T.isInfixOf "&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt; START" t

    isEnd :: Text -> Bool
    isEnd t = T.isInfixOf ">>>>>>>>>>\\ END" t
           || T.isInfixOf "&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt; END" t


    parseDumpKey :: Text -> DumpKey
    parseDumpKey
      = maybe (error "no key") (read . T.unpack)
      . T.stripPrefix "-- >>>>>>>>>> START "
      . case format of
          Format "latex" -> T.drop 18 . T.dropEnd 4
          Format "epub" -> T.drop 1 . T.dropWhile (not . (== '>')) . T.dropEnd 4
      . T.replace "\\ " " "
      . T.replace "&gt;" ">"

    go :: DumpKey -> [[Text]] -> [Text] -> Map DumpKey [Text]
    go key (acc : accs) (t : ts)
      | isStart t = go (parseDumpKey t) ([] : acc : accs) ts
      | isEnd t = M.insert key (reverse acc) $ go undefined accs ts
      | otherwise = go key ((t : acc) : accs) ts
    go _ _ _ = mempty


------------------------------------------------------------------------------
-- | code to test 'extractBetweenBanners'
_test :: Text
_test = T.unlines
  [ "\\>[0]\\AgdaComment{--\\ >>>>>>>>>>\\ START\\ DumpKey\\ Inline\\ 0}\\<%"
  , "a"
  , "b"
  , "\\>[0]\\AgdaComment{--\\ >>>>>>>>>>\\ END\\ DumpKey\\ Inline\\ 0}\\<%"
  ]


_test2 :: Text
_test2 = T.unlines
  [ "<a id=\"682\" class=\"Comment\">-- &gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt; START DumpKey Illegal (-3956750470541146748)</a>"
  , "    <a id=\"Example-TypingJudgments.illegal\"></a><a id=\"745\" href=\"Chapter1-Agda-dump.html#745\" class=\"Postulate\">illegal</a> <a id=\"753\" class=\"Symbol\">:</a> <a id=\"755\" href=\"Chapter1-Agda-dump.html#650\" class=\"Postulate\">false</a>"
  , "<a id=\"761\" class=\"Comment\">-- &gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt; END DumpKey Illegal (-3956750470541146748)</a>"
  ]

_test3 :: Text
_test3 = T.unlines
  [ "<a id=\"1424\" class=\"Comment\">-- &gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt; START DumpKey Inline (-810582888282262028)</a>"
  , "  <a id=\"1483\" href=\"Chapter1-Agda-dump.html#1483\" class=\"Function\">_</a> <a id=\"1485\" class=\"Symbol\">=</a> <a id=\"1487\" href=\"Chapter1-Agda-dump.html#1365\" class=\"Function\">not</a> <a id=\"1491\" href=\"Chapter1-Agda-dump.html#797\" class=\"InductiveConstructor\">false</a>"
  , "<a id=\"1497\" class=\"Comment\">-- &gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt;&gt; END DumpKey Inline (-810582888282262028)</a>"
  ]

------------------------------------------------------------------------------
-- | Splice a 'DumpKey' map back into a 'Block'; in essence, replacing the
-- 'IllegalRef' with the highlighted results.
spliceBlock :: Format -> Map DumpKey Text -> Block -> Block
spliceBlock format dk (IllegalRef key)
  | Just t <- M.lookup key dk
  = RawBlock (bool "html" "latex" $ format == Format "latex") t
spliceBlock _ _ b = b

------------------------------------------------------------------------------
-- | Splice a 'DumpKey' map back into a 'Inline'; in essence, replacing the
-- 'InlineRef' with the highlighted results.
spliceInline :: Format -> Map DumpKey Text -> Inline -> Inline
spliceInline format dk (InlineRef key)
  | Just t <- M.lookup key dk
  = RawInline (bool "html" "latex" $ format == Format "latex") t
spliceInline _ _ b = b


------------------------------------------------------------------------------
-- | Given the source of a highlighted Agda tex document, parse out the DumpKey
-- map corresponding to highlighted results.
parseHighlightedAgda :: Format -> Text -> Map DumpKey Text
parseHighlightedAgda format
  = M.mapWithKey (\k t ->
      case k of
        DumpKey Inline _ -> T.replace "%\n" "" t
        DumpKey Illegal _ ->
          case format of
            Format "latex" -> "\\begin{VERYILLEGALCODE}[indent=666]%\n%\n" <> t <> "\\end{VERYILLEGALCODE}"
            Format "epub"  -> "<illegal-code>\n\n" <> t <> "</illegal-code>"
            _ -> error "llegal format"
    )
  . fmap T.unlines
  . M.mapWithKey (\case
      DumpKey Inline _ ->
        case format of
          Format "latex" ->
            fmap ( T.replace "\\AgdaSpace{}" "~"
                 . T.replace "\\<" ""
                 )
            . init
            . drop 1
            . dropWhile (not . T.isInfixOf "\\AgdaSymbol{=}")
          Format "epub" ->
            T.lines
            . T.unwords
            . drop 1
            . dropWhile (not . T.isInfixOf "Symbol\">=</a>")
            . T.words
            . T.unlines
          Format _ -> error "impossible"
      DumpKey Illegal _ -> bool id (dropEnd 1 . drop 1) $ format == Format "latex"
                 )
  . extractBetweenBanners format
  . T.lines

