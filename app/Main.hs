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
import Data.List

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
    , liftK $ walk $ fixImages format
    , walkM inlineSnippets
    , walkM showCSV
    , walkM emitGhci
    , walkM citeLaw
    , liftK $ walk stripCodeTail
    , liftK $ walk $ ebookCode format
    , fmap pure compress
    -- , liftK $ walk $ defnToLatexEnv format "Exercise" "exercise"
    -- , liftK $ walk $ defnToLatexEnv format "Exercises" "exercise"
    , liftK $ walk $ quoteToDefn "TODO(sandy):" "TODO"
    , runIOorExplode . walkM (writeLatexDeathNotes format)
    , \p -> writeFile "/tmp/pandoc_output" (ppShow p) >> pure p
    ]


-- | Epub gets built in the wrong directory so this hack fixes that
fixImages :: Format -> Inline -> Inline
fixImages (Format "epub") (Image a b (c, d)) = Image a b ("build/" <> c, d)
fixImages _ t = t


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


ebookCode :: Format -> Block -> Block
ebookCode (Format "epub") (CodeBlock _ str) = RawBlock (Format "html") $ mconcat
  [ "<pre>"
  , T.pack $ replaceEbookCode $ T.unpack str
  , "</pre>"
  ]
ebookCode _ p = p

replaceEbookCode :: String -> String
replaceEbookCode [] = []
replaceEbookCode s
  | Just s' <- stripPrefix "-- .via " s
  = let (z, s'') = span (/= '\n') s'
     in mconcat
          [ "<span class=\"annotate\"><span class=\"fill\"></span><span class=\"annotated\">(via <span class=\"lawname\">"
          , z
          , "</span>)</span></span>"
          , replaceEbookCode s''
          ]
  | Just s' <- stripPrefix "-- ! " s
  = let (z, s'') = span (not . isSpace) s'
     in mconcat
          [ "<span class=\"annotate\"><span class=\"fill\"></span><span class=\"annotated\">"
          , pure $ toEnum $ fromEnum '\10102' + read z - 1
          , "</span></span>"
          , replaceEbookCode s''
          ]
replaceEbookCode (c:s) = c : replaceEbookCode s



stripCodeTail :: Block -> Block
stripCodeTail (CodeBlock a t) = CodeBlock a $ T.stripEnd t <> "\n"
stripCodeTail b = b


passes :: Pandoc -> [Pandoc -> IO Pandoc] -> IO Pandoc
passes p = flip appKEndo p . foldMap KEndo


liftK :: Applicative m => (a -> a) -> a -> m a
liftK f = pure . f

