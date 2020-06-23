{-# LANGUAGE LambdaCase #-}

module Cache (caching) where

import Data.Hashable
import Data.Maybe
import System.Directory


initCache :: IO ()
initCache = doesDirectoryExist cacheDir >>= \case
  True  -> pure ()
  False -> createDirectory cacheDir


caching :: (Hashable key, Show r, Read r) => key -> IO r -> IO r
caching key m = initCache >> checkCache key >>= \case
  Just x  -> pure x
  Nothing -> do
    r <- m
    writeCache key r
    pure r

writeCache :: (Hashable key, Show r) => key -> r -> IO ()
writeCache key = writeFile (hashFile key) . show

checkCache :: (Read r, Hashable key) => key -> IO (Maybe r)
checkCache key = doesFileExist (hashFile key) >>= \case
  False -> pure Nothing
  True -> safeRead <$> readFile (hashFile key)

hashFile :: Hashable key => key -> FilePath
hashFile key = cacheDir ++ show (hash key)

safeRead :: Read key => String -> Maybe key
safeRead = listToMaybe . fmap fst . reads

cacheDir :: FilePath
cacheDir = "./.design-tools"

