module Change where

import Prelude
import Prim hiding (Type)
import Syntax
import Utility

changeTerm :: TypeChange -> Term -> Term
changeTerm _ _ = unimplemented "changeTerm"

changeType :: TypeChange -> Type -> Type
changeType _ _ = unimplemented "changeType"
