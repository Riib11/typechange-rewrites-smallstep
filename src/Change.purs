module Change where

import Prelude
import Prim hiding (Type)
import Syntax
import Utility
import Data.Map as Map
import Data.Maybe (Maybe(..))

changeType :: TypeChange -> Type -> Type
changeType = case _ of
  ArrTypeChange arrCh -> case _ of
    ArrType arr -> ArrType arr { dom = changeType arrCh.dom arr.dom, cod = changeType arrCh.cod arr.cod }
    ty -> impossible $ "an arrow type change '" <> show (ArrTypeChange arrCh) <> "' cannot be applied to non arrow type '" <> show ty <> "'"
  ReplaceTypeChange rep -> \_old -> rep.new
  ArrowPlusTypeChange plus -> \cod -> ArrType { dom: plus.dom, cod: changeType plus.bod cod }
  ArrowMinusTypeChange minus -> case _ of
    ArrType arr -> changeType minus.bod arr.cod
    ty -> impossible $ "an arrow-minus type change '" <> show (ArrowMinusTypeChange minus) <> "' cannot be applied to non arrow type '" <> show ty <> "'"

changePolyType :: TypeChange -> PolyType -> PolyType
changePolyType tych (PolyType pty) = PolyType pty { ty = changeType tych pty.ty }

changeCtx :: CtxChange -> Ctx -> Ctx
changeCtx = case _ of
  TermVarCtxChange tmvarch -> case tmvarch.termVarChange of
    TypeChangeTermVarChange tych -> \ctx -> ctx { typings = Map.update (Just <<< changePolyType tych) tmvarch.termVar ctx.typings }
    DeleteVarChange _pty -> \ctx -> ctx { typings = Map.delete tmvarch.termVar ctx.typings }
    InsertVarChange pty -> \ctx -> ctx { typings = Map.insert tmvarch.termVar pty ctx.typings }
