{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# OPTIONS_GHC -Wall          #-}

module AgdaDump where

import Data.Hashable
import GHC.Generics
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad
import Data.Char (isSpace)
import Control.Lens (set)
import Control.Monad.State.Strict
import Control.Monad.Writer.CPS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Generics.Product.Fields
-- import Data.Monoid

data DumpSort
  = Invalid
  | Inline
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data DumpKey = DumpKey DumpSort Int
  deriving (Eq, Ord, Show, Read)

data St = St
  { st_indent :: Int
  }
  deriving (Generic)

type ExtractM = StateT St (Writer [Text])
-- deriving via (Ap ExtractM a) instance Semigroup a => Semigroup (ExtractM a)
-- deriving via (Ap ExtractM a) instance Monoid a => Monoid (ExtractM a)

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

getInvalidCode :: Block -> ExtractM Block
getInvalidCode c@(CodeBlock (_, cls, _) t)
  | not $ elem "invalid" cls = pure c
  | otherwise = do
      extractMe (DumpKey Invalid $ hash $ show c) $ tell [t]
      pure c
getInvalidCode c = pure c

getInlineCode :: Inline -> ExtractM Inline
getInlineCode c@(Code _ t)
  | Just t' <- T.stripPrefix "expr:" t =  do
      indent <- gets st_indent
      extractMe (DumpKey Inline $ hash $ show c) $ tell [T.replicate indent " " <> "_ = " <> t']
      pure c
getInlineCode c = pure c


runExtract :: ExtractM a -> IO a
runExtract m = do
  let (a, w) = runWriter $ flip evalStateT (St 0) m
  T.writeFile "/tmp/1-agda.lagda.tex" $ T.unlines w
  pure a


extractOf :: Pandoc -> IO Pandoc
extractOf p = do
  runExtract $ do
    tell ["\\begin{code}"]
    p' <- walkM getRealCode p
    tell ["\\end{code}"]
    pure p'

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
  . fmap (fmap (T.replace "\\<" "") . init . drop 4)
  . splitMe
  . T.lines


