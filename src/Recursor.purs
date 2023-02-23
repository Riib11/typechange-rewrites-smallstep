module Recursor where

import Change
import Prelude
import Prim hiding (Type)
import Syntax
import Utility
import Data.Foldable (foldrDefault)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))

{-
-- | recTerm
recTerm ::
  forall m a.
  Monad m =>
  { var :: { ctx :: Ctx, var :: VarTerm, pty :: PolyType, ty :: Type } -> m a
  , app :: { ctx :: Ctx, app :: AppTerm, tyDom :: Type, tyCod :: Type } -> m a
  , lam :: { ctx :: Ctx, lam :: LamTerm, tyDom :: Type, tyCod :: Type, ctxBod :: Ctx } -> m a
  , def :: { ctx :: Ctx, def :: DefTerm, tySig :: PolyType, ctxSig :: Ctx, ctxImp :: Ctx, ctxBod :: Ctx } -> m a
  , buf :: { ctx :: Ctx, tySig :: Type, buf :: BufTerm } -> m a
  , dat :: { ctx :: Ctx, dat :: DatTerm, kd :: Kind, constrPtys :: List { constr :: Constr, pty :: PolyType }, ctxConstrs :: Ctx, ctxBod :: Ctx } -> m a
  , tych :: { ctx :: Ctx, tychTm :: TypeChangeTerm, tych :: TypeChange, tyIn :: Type, tyOut :: Type } -> m a
  , ctxch :: { ctx :: Ctx, ctxch :: CtxChangeTerm, ctxIn :: Ctx } -> m a
  , hole :: { ctx :: Ctx, hole :: HoleTerm, ty :: Type } -> m a
  } ->
  Ctx -> Term -> m a
recTerm rec ctx = case _ of
  VarTerm var -> do
    let
      pty = lookupWithError var.termVar ctx.typings ("unknown term var '" <> show var.termVar <> "' in type context '" <> show ctx.typings <> "'")
    rec.var
      { ctx
      , var
      , pty
      , ty: monomorphizeType ctx pty (normType ctx <$> var.argTys)
      }
  AppTerm app ->
    infer ctx app.apl
      >>= case _ of
          ArrType { dom, cod } ->
            rec.app
              { ctx
              , app
              , tyDom: normType ctx dom
              , tyCod: normType ctx cod
              }
          ty -> impossible $ "mal-typed application: the applicant '" <> show app.apl <> "' has non-function type '" <> show ty <> "'"
  LamTerm lam -> do
    let
      ctxBod = ctx { typings = Map.insert lam.termVar (monoType lam.dom) ctx.typings }
    cod <- infer ctxBod lam.bod
    rec.lam
      { ctx
      , lam
      , tyDom: lam.dom
      , ctxBod: ctxBod
      , tyCod: cod
      }
  DefTerm def -> do
    let
      ctxSig =
        ctx
          { kindings =
            foldrDefault
              (\tyVar -> Map.insert tyVar baseKind)
              ctx.kindings
              ((unwrap def.sig).params)
          }

      ctxImp = ctxSig { typings = Map.insert def.termVar def.sig ctx.typings }
    rec.def
      { ctx
      , def
      , ctxSig
      , tySig: normPolyType ctxSig def.sig
      , ctxImp
      , ctxBod: ctx { typings = Map.insert def.termVar def.sig ctx.typings }
      }
  BufTerm buf ->
    rec.buf
      { ctx
      , tySig: normType ctx buf.sig
      , buf
      }
  DatTerm dat -> do
    let
      kd = kindFromParams dat.params

      -- introduce data type
      ctxConstrs = ctx { kindings = Map.insert dat.typeVar kd ctx.kindings }

      constrPolyType :: Constr -> PolyType
      constrPolyType constr =
        PolyType
          { params: dat.params
          , ty:
              ArrType
                { dom: (unwrap constr).ty
                , cod: NeuType { typeVar: dat.typeVar, args: varType <$> dat.params }
                }
          }

      constrPtys :: List { constr :: Constr, pty :: PolyType }
      constrPtys = (\constr -> { constr, pty: constrPolyType constr }) <$> dat.constrs

      -- introduce constructors
      ctxBod =
        ctxConstrs
          { typings =
            foldrDefault
              (\constr -> Map.insert (unwrap constr).termVar (constrPolyType constr))
              ctx.typings
              dat.constrs
          }
    rec.dat
      { ctx
      , dat
      , kd
      , constrPtys
      , ctxConstrs
      , ctxBod
      }
  TypeChangeTerm tychTm ->
    rec.tych
      { ctx
      , tychTm
      , tych: tychTm.tych
      , tyIn: typeChangeDomain tychTm.tych
      , tyOut: typeChangeCodomain tychTm.tych
      }
  CtxChangeTerm ctxch ->
    rec.ctxch
      { ctx
      , ctxch
      , ctxIn: inverseChangeCtx ctxch.ctxch ctx
      }
  HoleTerm hole ->
    rec.hole
      { ctx
      , hole
      , ty: normType ctx hole.ty
      }

infer :: forall m. Monad m => Ctx -> Term -> m Type
infer =
  recTerm
    { var: \args -> pure args.ty
    , app: \args -> pure args.tyCod
    , lam: \args -> pure $ ArrType { dom: args.tyDom, cod: args.tyCod }
    , def: \args -> infer args.ctxBod args.def.bod
    , buf: \args -> infer args.ctx args.buf.bod
    , dat: \args -> infer args.ctxBod args.dat.bod
    , tych: \args -> pure args.tyOut
    , ctxch: \args -> infer args.ctxIn args.ctxch.bod
    , hole: \args -> pure args.ty
    }

monomorphizeType :: Ctx -> PolyType -> List Type -> Type
monomorphizeType ctx (PolyType pty) tys =
  normType
    ( foldrDefault
        (\(tyVar /\ ty) ctx' -> ctx' { instances = Map.insert tyVar ty ctx'.instances })
        ctx
        (pty.params `List.zip` tys)
    )
    pty.ty

normPolyType :: Ctx -> PolyType -> PolyType
normPolyType ctx (PolyType pty) =
  PolyType
    pty
      { ty =
        normType
          (ctx { kindings = foldrDefault (\tyVar -> Map.insert tyVar baseKind) ctx.kindings pty.params })
          pty.ty
      }

normType :: Ctx -> Type -> Type
normType ctx = case _ of
  NeuType neu -> case Map.lookup neu.typeVar ctx.instances of
    Nothing -> NeuType neu { args = normType ctx <$> neu.args }
    -- instances in context should always be normal
    Just ty -> ty
  ArrType arr -> ArrType arr { dom = normType ctx arr.dom, cod = normType ctx arr.cod }
  HoleType hole -> HoleType hole
-}
