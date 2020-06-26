module Snippets where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T


matchDefinition :: String -> String -> Maybe ([String] -> [String])
matchDefinition decl line =
  listToMaybe $ do
    (form, f) <- [ ("", id)
                 , ("-- # ", tail)
                 , ("type family ", id)
                 , ("data family ", id)
                 , ("data ", id)
                 , ("type ", id)
                 , ("newtype ", id)
                 , ("class ", id)
                 ]
    guard $ isPrefixOf (form ++ decl) line
    pure f


getDefinition :: FilePath -> String -> Maybe String -> String
getDefinition fp file (Just decl)
    = unlines . func $ ls
  where
    ls = takeWhile (not . null)
       . dropWhile (isNothing . matchDefinition decl)
       $ lines file
    func = fromJust
         . matchDefinition decl
         . maybe (error $ "everything is fucked: " ++ fp ++ ":" ++ decl) id
         $ listToMaybe ls
getDefinition _ file Nothing = file

