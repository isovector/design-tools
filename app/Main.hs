module Main where

import Lib

main :: IO ()
main = passes
  [ expandAnnotations
  ]
