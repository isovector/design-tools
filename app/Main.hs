module Main where

import Combinators
import KEndo
import Lib
import Text.Pandoc.JSON
import Text.Pandoc.Walk


main :: IO ()
main = passes
  [ liftK $ walk $ linkToLatexCmd "Ann" "ann"
  , walkM inlineSnippets
  , walkM showCSV
  , liftK $ walk $ defnToLatexEnv "Exercise" "exercise"
  , liftK $ walk $ quoteToDefn "TODO(sandy):" "TODO"
  ]


passes :: [Pandoc -> IO Pandoc] -> IO ()
passes = toJSONFilter . appKEndo . foldMap KEndo


liftK :: Applicative m => (a -> a) -> a -> m a
liftK f = pure . f

