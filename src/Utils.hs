{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Utils where

import           CSV
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
isStr (Emph xs) = all isStr xs
isStr (Strong xs) = all isStr xs
isStr (Strikeout xs) = all isStr xs
isStr (Superscript xs) = all isStr xs
isStr (Subscript xs) = all isStr xs
isStr (SmallCaps xs) = all isStr xs
isStr (Link _ xs _) = all isStr xs
isStr (Code _ _) = True
isStr Space = True
isStr _ = False

fromStr :: Inline -> Text
fromStr (Str s) = s
fromStr (Emph xs) = foldMap fromStr xs
fromStr (Strong xs) = foldMap fromStr xs
fromStr (Strikeout xs) = foldMap fromStr xs
fromStr (Superscript xs) = foldMap fromStr xs
fromStr (Subscript xs) = foldMap fromStr xs
fromStr (SmallCaps xs) = foldMap fromStr xs
fromStr (Link _ xs _) = foldMap fromStr xs
fromStr (Code _ t) = t
fromStr Space = " "
