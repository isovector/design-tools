module CSV where

import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Map (Map)
import Data.Csv


loadCSV :: ByteString -> V.Vector (Map String String)
loadCSV = snd . either error id . decodeByName


filterCSV
    :: String
    -> String
    -> V.Vector (Map String String)
    -> V.Vector (Map String String)
filterCSV field value =
  V.filter ((== Just value) . M.lookup field)


selectCSV
    :: String
    -> V.Vector (Map String String)
    -> V.Vector String
selectCSV field = V.mapMaybe (M.lookup field)

