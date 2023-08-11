{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# OPTIONS_GHC -Wall          #-}

module AgdaDump where

import Control.Monad
import GHC.Generics

import Data.Char (isSpace)
import Control.Lens (over, set)
import Control.Monad.State.Strict
import Control.Monad.Writer.CPS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Generics.Product.Fields
-- import Data.Monoid

data St = St
  { st_next   :: Int
  , st_indent :: Int
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


getNextExtract :: ExtractM Int
getNextExtract = gets st_next <* modify (over (field @"st_next") (+1))

mkBanner :: Text -> Text -> Int -> Text
mkBanner z sort ix = mconcat
  [ "-- >>>>>>>>>> "
  , z
  , " "
  , sort
  , " "
  , T.pack $ show ix
  ]

extractMe :: Text -> ExtractM a -> ExtractM a
extractMe sort m = do
  ix <- getNextExtract
  tell . pure $ mkBanner "START" sort ix
  r <- m
  tell . pure $ mkBanner "END" sort ix
  pure r

getInvalidCode :: Block -> ExtractM Block
getInvalidCode c@(CodeBlock (_, cls, _) t)
  | not $ elem "invalid" cls = pure c
  | otherwise = do
      extractMe "INVALID" $ tell [t]
      pure c
getInvalidCode c = pure c

getInlineCode :: Inline -> ExtractM Inline
getInlineCode c@(Code _ t)
  | Just t' <- T.stripPrefix "expr:" t =  do
      indent <- gets st_indent
      extractMe "INLINE" $ tell [T.replicate indent " " <> "_ = " <> t']
      pure c
getInlineCode c = pure c


runExtract :: ExtractM a -> IO a
runExtract m = do
  let (a, w) = runWriter $ flip evalStateT (St 0 0) m
  T.writeFile "/tmp/1-agda.lagda.tex" $ T.unlines w
  pure a


extractOf :: Pandoc -> IO Pandoc
extractOf p = do
  runExtract $ do
    tell ["\\begin{code}"]
    p' <- walkM getRealCode p
    tell ["\\end{code}"]
    pure p'


splitMe :: [Text] -> [[Text]]
splitMe = go [[]]
  where
    isStart :: Text -> Bool
    isStart = T.isInfixOf "START"

    isEnd :: Text -> Bool
    isEnd = T.isInfixOf "END"

    go :: [[Text]] -> [Text] -> [[Text]]
    go (acc : accs) (t : ts)
      | isStart t = go ([] : acc : accs) ts
      | isEnd t = reverse acc : go accs ts
      | otherwise = go ((t : acc) : accs) ts
    go _ _ = []


test :: Text
test = T.unlines
  [ "START"
  , "a"
  , "b"
  , "END"
  , "c"
  , "START"
  , "d"
  , "END"
  ]

parseHighlightedAgda :: Text -> [Text]
parseHighlightedAgda
  = fmap T.unlines
  . fmap (fmap (T.replace "\\<" "") . init . drop 4)
  . splitMe
  . T.lines


