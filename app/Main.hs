{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Data.Char hiding (Format, Space)
import qualified Data.Text as T
import Ghci
import Combinators
import KEndo
import Lib
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import DeathNotes
import Text.Pandoc
import System.IO

import Text.Show.Pretty


main :: IO ()
main = toJSONFilter $ \(Just format :: Maybe Format) (p :: Pandoc) -> do
  hPrint stderr format
  passes p
    [ \p -> writeFile "/tmp/pandoc_input" (ppShow p) >> pure p
    , liftK $ walk $ linkToLatexCmd format "Ann" "ann"
    , liftK $ walk $ prefixCodeToLatexCmd format "law:" "lawname"
    , liftK $ walk $ wrapCodeEnv format "haskell" "EqnBox" $ Just "law"
    , liftK $ walk $ noIndent format
    , walkM inlineSnippets
    , walkM showCSV
    , walkM emitGhci
    , walkM citeLaw
    , fmap pure compress
    -- , liftK $ walk $ defnToLatexEnv format "Exercise" "exercise"
    -- , liftK $ walk $ defnToLatexEnv format "Exercises" "exercise"
    , liftK $ walk $ quoteToDefn "TODO(sandy):" "TODO"
    , runIOorExplode . walkM (writeLatexDeathNotes format)
    , \p -> writeFile "/tmp/pandoc_output" (ppShow p) >> pure p
    ]


compress :: Pandoc -> Pandoc
compress (Pandoc meta blocks) = Pandoc meta $ go blocks
  where
    go ( CodeBlock (cb_id, ["haskell"], kvs) str1
       : CodeBlock (_, ["haskell"], _) str2
       : cs
       ) = go $ CodeBlock (cb_id, ["haskell"], kvs)
                  (str1 <> "\n\n" <> str2)
              : cs
    go (x : xs) = x : go xs
    go [] = []


noIndent :: Format -> Block -> Block
noIndent (Format "latex") p@(Para pc@(Str str : _)) =
  case fmap isLower $ T.unpack $ T.take 1 str of
    [True] -> Para $ RawInline (Format "latex") "\\noindent" : Space : pc
    _ -> p
noIndent _ p = p


passes :: Pandoc -> [Pandoc -> IO Pandoc] -> IO Pandoc
passes p = flip appKEndo p . foldMap KEndo


liftK :: Applicative m => (a -> a) -> a -> m a
liftK f = pure . f

