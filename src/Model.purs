module Model where

import Prelude
import Data.Lazy (Lazy)
import Halogen.Query.HalogenM (SubscriptionId(..))
import Syntax (Term)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

type Model
  = { term :: Term
    , edits :: Array Edit
    }

data Action
  = SetTermAction (Lazy Term)
  | SetEditsAction MouseEvent (Array Edit)
  | StepAction
  | KeyDownAction SubscriptionId KeyboardEvent
  | InitializeAction

type Edit
  = { label ∷ String, term ∷ Lazy Term }
