module Main where

import Combinators
import KEndo
import Lib
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import DeathNotes
import Text.Pandoc


main :: IO ()
main = passes
  [ liftK $ walk $ linkToLatexCmd "Ann" "ann"
  , liftK $ walk $ prefixCodeToLatexCmd "law:" "lawname"
  , walkM inlineSnippets
  , walkM showCSV
  , liftK $ walk $ defnToLatexEnv "Exercise" "exercise"
  , liftK $ walk $ quoteToDefn "TODO(sandy):" "TODO"
  , runIOorExplode . walkM writeLatexDeathNotes
  ]


passes :: [Pandoc -> IO Pandoc] -> IO ()
passes = toJSONFilter . appKEndo . foldMap KEndo


liftK :: Applicative m => (a -> a) -> a -> m a
liftK f = pure . f

