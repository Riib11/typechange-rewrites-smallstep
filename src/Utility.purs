module Utility where

import Prelude
import Effect.Exception.Unsafe (unsafeThrow)

unimplemented :: forall a. String -> a
unimplemented str = unsafeThrow ("unimplemented: " <> str)
