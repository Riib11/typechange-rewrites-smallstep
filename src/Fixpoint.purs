module Fixpoint where

import Prelude
import Data.Maybe (Maybe(..))

fixpoint :: forall a. (a -> Maybe a) -> a -> a
fixpoint k a = case k a of
  Nothing -> a
  Just a' -> fixpoint k a'

fixpointM :: forall m a. Monad m => (a -> m (Maybe a)) -> a -> m a
fixpointM k a =
  k a
    >>= case _ of
        Nothing -> pure a
        Just a' -> fixpointM k a'
