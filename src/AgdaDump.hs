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

module AgdaDump where

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


data DumpSort
  = Invalid
  | Inline
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

data DumpKey = DumpKey DumpSort Int
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable DumpSort
instance Hashable DumpKey

data St = St
  { st_indent :: Int
  }
  deriving (Generic)

type ExtractM = StateT St (WriterT [Text] (Writer [DumpKey]))

getModule :: Block -> First Text
getModule (CodeBlock (_, cls, _) t)
  | elem "agda" cls = flip foldMap (T.lines t) $ \l ->
      case T.stripPrefix "module" l of
        Just m -> pure $ head $ T.words m
        Nothing -> mempty
getModule _ = mempty


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


getNextExtract :: Hashable a => a -> ExtractM Int
getNextExtract = pure . hash

mkBanner :: Text -> DumpKey -> Text
mkBanner z key = mconcat
  [ "-- >>>>>>>>>> "
  , z
  , " "
  , T.pack $ show key
  ]

extractMe :: DumpKey -> ExtractM a -> ExtractM a
extractMe key m = do
  tell . pure $ mkBanner "START" key
  r <- m
  tell . pure $ mkBanner "END" key
  pure r

pattern InvalidRef :: DumpKey -> Block
pattern InvalidRef key <- CodeBlock _ (readMaybe . T.unpack -> Just key)
  where
    InvalidRef = CodeBlock ("", [], []) . T.pack . show

pattern InlineRef :: DumpKey -> Inline
pattern InlineRef key <- Code _ (readMaybe . T.unpack -> Just key)
  where
    InlineRef = Code ("", [], []) . T.pack . show

getInvalidCode :: Block -> ExtractM Block
getInvalidCode c@(CodeBlock (_, cls, _) t)
  | not $ elem "invalid" cls = pure c
  | otherwise = do
      let key = DumpKey Invalid $ hash $ show c
      extractMe key $ tell [t]
      pure $ InvalidRef key
getInvalidCode c = pure c

getInlineCode :: Inline -> ExtractM Inline
getInlineCode c@(Code _ t)
  | Just t' <- T.stripPrefix "expr:" t =  do
      indent <- gets st_indent
      let key = DumpKey Inline $ hash $ show c
      extractMe key $ tell [T.replicate indent " " <> "_ = " <> t']
      pure $ InlineRef key
getInlineCode c = pure c


runExtract :: Text -> ExtractM a -> IO ([DumpKey], a)
runExtract modul m = do
  let ((a, w), keys) = runWriter $ runWriterT $ flip evalStateT (St 0) m
      modul' = modul <> "-dump"
      out = T.replace modul modul' $ T.unlines w
      keyset = sort keys
  T.writeFile ("/tmp/" <> T.unpack modul' <> ".lagda.tex") out
  pure (keyset, a)


extractOf :: Pandoc -> IO Pandoc
extractOf p = do
  let First (Just modul) = query getModule p
      modul' = modul <> "-dump"

  (keyset, p') <- runExtract modul $ do
    tell ["\\begin{code}"]
    p' <- walkM getRealCode p
    tell ["\\end{code}"]
    pure p'

  dump <- caching keyset $ do
    _ <- withCurrentDirectory "/tmp"
       $ readProcess "agda" ["--latex", T.unpack modul' <> ".lagda.tex"] ""
    f <- T.readFile $ "/tmp/latex/" <> T.unpack modul' <> ".tex"
    pure $ parseHighlightedAgda f

  pure $ walk (spliceInline dump) $ walk (spliceBlock dump) p'

splitMe :: [Text] -> Map DumpKey [Text]
splitMe = go undefined [[]]
  where
    isStart :: Text -> Bool
    isStart = T.isInfixOf ">>>>>>>>>>\\ START"

    isEnd :: Text -> Bool
    isEnd = T.isInfixOf ">>>>>>>>>>\\ END"

    getKey :: Text -> DumpKey
    getKey
      = maybe (error "no key") (read . T.unpack)
      . T.stripPrefix "-- >>>>>>>>>> START "
      . T.drop 18
      . T.dropEnd 4
      . T.replace "\\ " " "

    go :: DumpKey -> [[Text]] -> [Text] -> Map DumpKey [Text]
    go key (acc : accs) (t : ts)
      | isStart t = go (getKey t) ([] : acc : accs) ts
      | isEnd t = M.insert key (reverse acc) $ go undefined accs ts
      | otherwise = go key ((t : acc) : accs) ts
    go _ _ _ = mempty


spliceBlock :: Map DumpKey Text -> Block -> Block
spliceBlock dk (InvalidRef key)
  | Just t <- M.lookup key dk
  = RawBlock "latex" t
spliceBlock _ b = b

spliceInline :: Map DumpKey Text -> Inline -> Inline
spliceInline dk (InlineRef key)
  | Just t <- M.lookup key dk
  = RawInline "latex" t
spliceInline _ b = b



test :: Text
test = T.unlines
  [ "\\>[0]\\AgdaComment{--\\ >>>>>>>>>>\\ START\\ DumpKey\\ Inline\\ 0}\\<%"
  , "a"
  , "b"
  , "\\>[0]\\AgdaComment{--\\ >>>>>>>>>>\\ END\\ DumpKey\\ Inline\\ 0}\\<%"
  ]

parseHighlightedAgda :: Text -> Map DumpKey Text
parseHighlightedAgda
  = fmap T.unlines
  . fmap (fmap (T.replace "\\AgdaSpace{}" "~" . T.replace "\\<" "") . init . drop 4)
  . splitMe
  . T.lines

