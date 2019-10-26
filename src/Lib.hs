{-# LANGUAGE LambdaCase #-}

module Lib where

import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.JSON
import Data.Monoid


expandAnnotations :: Pandoc -> Pandoc
expandAnnotations = walk $ \case
  Link _ [Str t] ("Ann", _) ->
    RawInline (Format "latex") $ "\\ann{" ++ t ++ "}"
  t -> t


passes :: [Pandoc -> Pandoc] -> IO ()
passes = toJSONFilter . appEndo . foldMap Endo


