module Main where

import KEndo
import Lib
import Text.Pandoc.JSON
import Text.Pandoc.Walk


main :: IO ()
main = passes
  [ liftK $ walk expandAnnotations
  , walkM inlineCode
  ]


passes :: [Pandoc -> IO Pandoc] -> IO ()
passes = toJSONFilter . appKEndo . foldMap KEndo

liftK :: Applicative m => (a -> a) -> a -> m a
liftK f = pure . f

