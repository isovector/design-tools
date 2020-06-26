module CSV where

import           Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Map (Map)
import Data.Csv


loadCSV :: ByteString -> V.Vector (Map Text Text)
loadCSV = snd . either error id . decodeByName


filterCSV
    :: Text
    -> Text
    -> V.Vector (Map Text Text)
    -> V.Vector (Map Text Text)
filterCSV field value =
  V.filter ((== Just value) . M.lookup field)


selectCSV
    :: Text
    -> V.Vector (Map Text Text)
    -> V.Vector Text
selectCSV field = V.mapMaybe (M.lookup field)

