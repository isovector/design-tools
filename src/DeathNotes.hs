{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module DeathNotes where

import Text.Pandoc.Definition
import Text.Pandoc
import Data.List
import qualified Data.Text as T


pattern DeathNote :: [Block] -> Inline
pattern DeathNote bs <- Note (splitDeathNote -> Just bs)
  where
    DeathNote (Para ps : bs) = Note $ Para (Str "death" : Space : ps) : bs
    DeathNote bs             = Note $ Para [Str "death"] : bs


splitDeathNote :: [Block] -> Maybe [Block]
splitDeathNote (Para (Str "death" : ps) : bs)
  = Just (Para (dropWhile (== Space) ps) : bs)
splitDeathNote _ = Nothing


writeLatexDeathNotes :: PandocMonad m => Format -> Inline -> m Inline
writeLatexDeathNotes _ (DeathNote bs) = do
  res <- writeLaTeX def {writerExtensions = pandocExtensions }
       $ Pandoc mempty bs
  pure $
    RawInline (Format "latex") $ mconcat
      [ "\\DeathNote{"
      , T.unpack res
      , "}"
      ]
writeLatexDeathNotes _ x = pure x

