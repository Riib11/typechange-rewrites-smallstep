module Utility where

import Data.Array
import Prelude
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafeCrashWith)

lookupWithError ∷ ∀ k v. Ord k ⇒ k → Map.Map k v → String → v
lookupWithError k m msg = case Map.lookup k m of
  Nothing -> impossible ("lookupWithError: " <> msg)
  Just v -> v

showUUID :: UUID.UUID -> String
showUUID uuid = String.take 2 (UUID.toString uuid)

unimplemented :: forall a. String -> a
unimplemented str = unsafeThrow ("unimplemented: " <> str)

impossible :: forall a. String -> a
impossible str = unsafeThrow ("impossible: " <> str)

-- curry/uncurry for arrays or arguments instead of tuples
arraycurry1 ∷ ∀ (t18 ∷ Type) (t20 ∷ Type). (Array t18 → t20) → t18 → t20
arraycurry1 f a1 = f [ a1 ]

arrayuncurry1 ∷ ∀ (t10 ∷ Type) (t11 ∷ Type). (t11 → t10) → Array t11 → t10
arrayuncurry1 f = case _ of
  [ a1 ] -> f a1
  _ -> unsafeThrow "`arrayuncurry1` was given an array with the wrong number of arguments"

arraycurry2 ∷ ∀ a b. (Array a → b) → (a → a → b)
arraycurry2 f a1 a2 = f [ a1, a2 ]

arrayuncurry2 ∷ ∀ (b ∷ Type) (a ∷ Type). (a → a → b) → Array a → b
arrayuncurry2 f = case _ of
  [ a1, a2 ] -> f a1 a2
  _ -> unsafeThrow "`arrayuncurry2` was given an array with the wrong number of arguments"

arraycurry3 ∷ ∀ (a ∷ Type) (c ∷ Type). (Array a → c) → a → a → a → c
arraycurry3 f a1 a2 a3 = f [ a1, a2, a3 ]

arrayuncurry3 ∷ ∀ (t6 ∷ Type) (t7 ∷ Type). (t7 → t7 → t7 → t6) → Array t7 → t6
arrayuncurry3 f = case _ of
  [ a1, a2, a3 ] -> f a1 a2 a3
  _ -> unsafeThrow "`arrayuncurry3` was given an array with the wrong number of arguments"
