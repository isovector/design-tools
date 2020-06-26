{-# LANGUAGE LambdaCase #-}

module Ghci (emitGhci) where

import Cache
import Control.Lens
import Data.Bool
import Data.List
import System.Process
import Text.Pandoc.JSON
import           Data.Text (Text)
import qualified Data.Text as T



emitGhci :: Block -> IO Block
emitGhci (CodeBlock (_, _, cs) str)
  | Just file <- lookup "ghci" cs
  = caching (file, str) $ ghciToPandoc (T.unpack file) (T.unpack str)
emitGhci x = pure x


ghciToPandoc :: FilePath -> String -> IO Block
ghciToPandoc fp = fmap format . runGhci fp


format :: [(String, String)] -> Block
format
  = CodeBlock ("", ["haskell", "ghci"], [])
  . T.pack
  . unlines
  . fmap (\(req, resp) -> unlines ["> " ++ req, resp])


runGhci :: FilePath -> String -> IO [(String, String)]
runGhci fp str
  = fmap (zip (lines str) . responses)
  . readProcess "stack" ["repl"]
  $ unlines [":l " ++ fp, str]


responses :: String -> [String]
responses
  = fmap unlines
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

