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
import Agda

import Text.Show.Pretty


main :: IO ()
main = toJSONFilter $ \(Just format :: Maybe Format) (p :: Pandoc) -> do
  hPrint stderr format
  passes p
    [ \p -> writeFile "/tmp/pandoc_input" (ppShow p) >> pure p
    , label "agdamode" $ liftK $ walk $ linkToLatexCmd format "AgdaMode" "agdamode"
    , label "agdacmds" $ liftK $ walk $ agdaCmd format
    , label "info" $ liftK $ walk $ codeToEnv format "info" "AgdaInfo"
    -- , label "illegal" $ liftK $ walk $ renameCode format "illegal" "agda"
    , label "anns" $ liftK $ walk $ linkToLatexCmd format "Ann" "ann"
    , label "rev2" $ liftK $ walk $ headerClassAppend format "rev2" "red"
    , label "laws" $ liftK $ walk $ prefixCodeToLatexCmd format "law:" "lawname"
    , label "types" $ liftK $ walk $ prefixCodeToLatexCmd format "type:" "AgdaFunction"
    , label "defs" $ liftK $ walk $ prefixCodeToLatexCmd format "def:" "AgdaFunction"
    , label "fields" $ liftK $ walk $ prefixCodeToLatexCmd format "field:" "AgdaField"
    , label "macro" $ liftK $ walk $ prefixCodeToLatexCmd format "macro:" "AgdaMacro"
    , label "constructors" $ liftK $ walk $ prefixCodeToLatexCmd format "ctor:" "AgdaInductiveConstructor"
    , label "modules" $ liftK $ walk $ prefixCodeToLatexCmd format "module:" "AgdaModule"
    , label "keywords" $ liftK $ walk $ prefixCodeToLatexCmd format "keyword:" "AgdaKeyword"
    , label "eqnbox" $ liftK $ walk $ wrapCodeEnv format "haskell" "EqnBox" $ Just "law"
    , label "noindent" $ liftK $ walk $ noIndent format
    , label "fix imgs" $ liftK $ walk $ fixImages format
    , label "hidden" $ liftK $ walk $ defnToLatexEnv format "Hidden" "hidden"
    , label "defns to laws" $ liftK $ walk $ defnToLatexEnv format "FlushRight" "flushright"
    , label "br" $ liftK $ walk $ br format "sdcp:br"
    , label "inline snippets" $ walkM inlineSnippets
    , label "do ghci" $ walkM emitGhci
    , label "strip code" $ liftK $ walk stripCodeTail
    , label "ebook" $ liftK $ walk $ ebookCode format
    , label "only book" $ liftK $ walk $ defnOnlyInFormat format "latex" "OnlyBook"
    , fmap pure compress
    , liftK $ walk $ defnToLatexEnv format "Exercise" "exercise"
    , liftK $ walk $ defnToLatexEnv format "Exercises" "exercise"
    , liftK $ walk $ quoteToDefn "TODO(sandy):" "TODO"
    , runIOorExplode . walkM (writeLatexDeathNotes format)
    , \p -> writeFile "/tmp/pandoc_output" (ppShow p) >> pure p
    ]


label :: String -> (t -> IO b) -> t -> IO b
label n w = \p -> hPutStrLn stderr n >> w p


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
  , T.pack $ replaceEbookCode $ stringToHtmlString $ T.unpack str
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

-- from https://www.stackage.org/haddock/lts-16.13/xhtml-3000.2.2.1/src/Text-XHtml-Internals.html
stringToHtmlString :: String -> String
stringToHtmlString = concatMap fixChar
  where
    fixChar '<' = "&lt;"
    fixChar '>' = "&gt;"
    fixChar '&' = "&amp;"
    fixChar '"' = "&quot;"
    fixChar c | ord c < 0x80 = [c]
    fixChar c = "&#" ++ show (ord c) ++ ";"


stripCodeTail :: Block -> Block
stripCodeTail (CodeBlock a t) = CodeBlock a $ T.stripEnd t <> "\n"
stripCodeTail b = b


passes :: Pandoc -> [Pandoc -> IO Pandoc] -> IO Pandoc
passes p = flip appKEndo p . foldMap KEndo


liftK :: Applicative m => (a -> a) -> a -> m a
liftK f = pure . f

