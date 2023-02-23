module Change where

import Prelude
import Prim hiding (Type)
import Syntax
import Utility

changeTerm :: TypeChange -> Term -> Term
changeTerm _ _ = unimplemented "changeTerm"

changeType :: TypeChange -> Type -> Type
changeType _ _ = unimplemented "changeType"

changePolyType :: TypeChange -> PolyType -> PolyType
changePolyType tych (PolyType pty) = PolyType pty { ty = changeType tych pty.ty }

-- applies the inverse of the given context change to the context
inverseChangeCtx :: CtxChange -> Ctx -> Ctx
inverseChangeCtx _ _ = unimplemented "inverseChangeCtx"
