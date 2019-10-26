module KEndo where

import Control.Monad

newtype KEndo m a = KEndo
  { appKEndo :: (a -> m a)
  }

instance Monad m => Semigroup (KEndo m a) where
  KEndo a <> KEndo b = KEndo $ a >=> b

instance Monad m => Monoid (KEndo m a) where
  mempty = KEndo pure

