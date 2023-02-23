module Syntax where

import Prelude
import Prim hiding (Type)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.UUID (UUID, genUUID)
import Data.UUID as UUID
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Unsafe (unsafePerformEffect)
import Utility (showUUID, unimplemented)

-- | Kind
newtype Kind
  = MakeKind Int

derive instance genericKind :: Generic Kind _

instance showKind :: Show Kind where
  show (MakeKind n)
    | n == 0 = "*"
    | n > 0 = "* -> " <> show (MakeKind (n - 1))
    | otherwise = unsafeThrow $ "invalid kind"

baseKind :: Kind
baseKind = MakeKind 0

kindFromParams :: List TypeVar -> Kind
kindFromParams tyVars = MakeKind (List.length tyVars)

-- | PolyType
newtype PolyType
  = PolyType { params :: List TypeVar, ty :: Type }

monoType :: Type -> PolyType
monoType ty = PolyType { params: mempty, ty }

derive instance genericPolyType :: Generic PolyType _

derive instance newtypePolyType :: Newtype PolyType _

instance showPolyType :: Show PolyType where
  show x = genericShow x

-- | Type 
data Type
  = NeuType NeuType
  | ArrType ArrType
  | HoleType HoleType

derive instance genericType :: Generic Type _

instance eqType :: Eq Type where
  eq x y = genericEq x y

instance showType :: Show Type where
  show x = genericShow x

type NeuType
  = { typeVar :: TypeVar, args :: List Type }

type ArrType
  = { dom :: Type, cod :: Type }

type HoleType
  = { typeHole :: TypeHole }

varType :: TypeVar -> Type
varType typeVar = NeuType { typeVar, args: mempty }

freshHoleType :: Unit -> Type
freshHoleType _ = HoleType { typeHole: freshTypeHole unit }

-- | Term
data Term
  = VarTerm VarTerm
  | AppTerm AppTerm
  | LamTerm LamTerm
  | DefTerm DefTerm
  | BufTerm BufTerm
  | DatTerm DatTerm
  | TypeChangeTerm TypeChangeTerm
  | CtxChangeTerm CtxChangeTerm
  | HoleTerm HoleTerm

derive instance genericTerm :: Generic Term _

instance showTerm :: Show Term where
  show x = genericShow x

type VarTerm
  = { termVar :: TermVar, argTys :: List Type, ty :: Type }

type AppTerm
  = { apl :: Term, arg :: Term, ty :: Type }

type LamTerm
  = { termVar :: TermVar, dom :: Type, bod :: Term }

type DefTerm
  = { termVar :: TermVar, sig :: PolyType, imp :: Term, bod :: Term }

type DatTerm
  = { typeVar :: TypeVar, params :: List TypeVar, constrs :: List Constr, bod :: Term }

type BufTerm
  = { sig :: Type, imp :: Term, bod :: Term }

type TypeChangeTerm
  = { dir :: Direction, tych :: TypeChange, bod :: Term }

type CtxChangeTerm
  = { dir :: Direction, ctxch :: CtxChange, bod :: Term }

type HoleTerm
  = { termHole :: TermHole, ty :: Type }

freshHoleTerm :: Type -> Term
freshHoleTerm ty = HoleTerm { termHole: freshTermHole unit, ty }

infer :: Term -> Type
infer (VarTerm var) = var.ty

infer (AppTerm app) = app.ty

infer (LamTerm lam) = ArrType { dom: lam.dom, cod: infer lam.bod }

infer (DefTerm def) = infer def.bod

infer (BufTerm buf) = infer buf.bod

infer (DatTerm dat) = infer dat.bod

infer (TypeChangeTerm tych) = typeChangeCodomain tych.tych

infer (CtxChangeTerm ctxch) = infer ctxch.bod

infer (HoleTerm hole) = hole.ty

-- | Constr
newtype Constr
  = Constr { termVar :: TermVar, ty :: Type }

derive instance genericConstr :: Generic Constr _

derive instance newtypeConstr :: Newtype Constr _

instance showConstr :: Show Constr where
  show x = genericShow x

-- | TypeChange
data TypeChange
  = ArrTypeChange ArrTypeChange
  | ReplaceTypeChange ReplaceTypeChange
  | ArrowPlusTypeChange ArrowPlusTypeChange
  | ArrowMinusTypeChange ArrowMinusTypeChange

-- TODO: add type changes for polymorphic types type vars and stuff
derive instance genericTypeChange :: Generic TypeChange _

instance showTypeChange :: Show TypeChange where
  show x = genericShow x

type ArrTypeChange
  = { dom :: TypeChange, cod :: TypeChange }

type ArrowPlusTypeChange
  = { dom :: Type, bod :: TypeChange }

type ArrowMinusTypeChange
  = { dom :: Type, bod :: TypeChange }

type NeuTypeChange
  = { typeVar :: TypeVar, params :: List ParamChange }

type HoleChange
  = { typeHole :: TypeHole }

type ReplaceTypeChange
  = { old :: Type, new :: Type }

identityTypeChange :: Type -> TypeChange
identityTypeChange ty = ReplaceTypeChange { old: ty, new: ty }

domainTypeChange :: TypeChange -> Type -> TypeChange
domainTypeChange dom cod = ArrTypeChange { dom, cod: identityTypeChange cod }

codomainTypeChange :: Type -> TypeChange -> TypeChange
codomainTypeChange dom cod = ArrTypeChange { dom: identityTypeChange dom, cod }

typeChangeDomain :: TypeChange -> Type
typeChangeDomain = case _ of
  ArrTypeChange { dom, cod } -> ArrType { dom: typeChangeDomain dom, cod: typeChangeDomain cod }
  ReplaceTypeChange { old } -> old
  ArrowPlusTypeChange { bod } -> typeChangeDomain bod
  ArrowMinusTypeChange { bod } -> typeChangeDomain bod

typeChangeCodomain :: TypeChange -> Type
typeChangeCodomain = case _ of
  ArrTypeChange { dom, cod } -> ArrType { dom: typeChangeCodomain dom, cod: typeChangeCodomain cod }
  ReplaceTypeChange { old } -> old
  ArrowPlusTypeChange { dom, bod } -> ArrType { dom, cod: typeChangeCodomain bod }
  ArrowMinusTypeChange { bod } -> case typeChangeCodomain bod of
    ArrType { cod } -> cod
    ty -> unsafeThrow $ "ArrowMinusTypeChange of non-Arrow type: " <> show ty

invertTypeChange :: TypeChange -> TypeChange
invertTypeChange = case _ of
  ArrTypeChange arr -> ArrTypeChange { dom: invertTypeChange arr.dom, cod: invertTypeChange arr.cod }
  ReplaceTypeChange rep -> ReplaceTypeChange { old: rep.new, new: rep.old }
  ArrowPlusTypeChange plus -> ArrowMinusTypeChange { dom: plus.dom, bod: plus.bod }
  ArrowMinusTypeChange minus -> ArrowPlusTypeChange { dom: minus.dom, bod: minus.bod }

-- | ParamChange
data ParamChange
  = ParamChange TypeChange
  | PlusParamChange Type
  | MinusParamChange Type

derive instance genericParamChange :: Generic ParamChange _

instance showParamChange :: Show ParamChange where
  show x = genericShow x

-- | CtxChange
data CtxChange
  = TermVarCtxChange TermVarCtxChange

derive instance genericCtxChange :: Generic CtxChange _

instance showCtxChange :: Show CtxChange where
  show x = genericShow x

type TermVarCtxChange
  = { termVar :: TermVar, termVarChange :: TermVarChange }

invertCtxChange :: CtxChange -> CtxChange
invertCtxChange = case _ of
  TermVarCtxChange tmvarch ->
    TermVarCtxChange
      { termVar: tmvarch.termVar
      , termVarChange: invertTermVarChange tmvarch.termVarChange
      }

-- | TermVarChange
data TermVarChange
  = TypeChangeTermVarChange TypeChange
  | DeleteVarChange PolyType
  | InsertVarChange PolyType

derive instance genericTermVarChange :: Generic TermVarChange _

instance showTermVarChange :: Show TermVarChange where
  show x = genericShow x

invertTermVarChange :: TermVarChange -> TermVarChange
invertTermVarChange = case _ of
  TypeChangeTermVarChange tych -> TypeChangeTermVarChange $ invertTypeChange tych
  DeleteVarChange pty -> InsertVarChange pty
  InsertVarChange pty -> DeleteVarChange pty

-- | TypeVar
newtype TypeVar
  = MakeTypeVar UUID

derive instance genericTypeVar :: Generic TypeVar _

derive newtype instance eqTypeVar :: Eq TypeVar

derive newtype instance ordTypeVar :: Ord TypeVar

instance showTypeVar :: Show TypeVar where
  show (MakeTypeVar uuid) = "?" <> showUUID uuid

freshTypeVar :: Unit -> TypeVar
freshTypeVar _ = unsafePerformEffect $ MakeTypeVar <$> genUUID

-- | TermVar 
newtype TermVar
  = MakeTermVar UUID

derive instance genericTermVar :: Generic TermVar _

derive newtype instance eqTermVar :: Eq TermVar

derive newtype instance ordTermVar :: Ord TermVar

instance showTermVar :: Show TermVar where
  show (MakeTermVar uuid) = "?" <> showUUID uuid

freshTermVar :: Unit -> TermVar
freshTermVar _ = unsafePerformEffect $ MakeTermVar <$> genUUID

-- | TypeHole
newtype TypeHole
  = MakeTypeHole UUID

derive instance genericTypeHole :: Generic TypeHole _

derive newtype instance eqTypeHole :: Eq TypeHole

derive newtype instance ordTypeHole :: Ord TypeHole

instance showTypeHole :: Show TypeHole where
  show (MakeTypeHole uuid) = "?" <> showUUID uuid

freshTypeHole :: Unit -> TypeHole
freshTypeHole _ = unsafePerformEffect $ MakeTypeHole <$> genUUID

-- | TermHole
newtype TermHole
  = MakeTermHole Unit

derive instance genericTermHole :: Generic TermHole _

derive newtype instance eqTermHole :: Eq TermHole

derive newtype instance ordTermHole :: Ord TermHole

instance showTermHole :: Show TermHole where
  show x = genericShow x

freshTermHole :: Unit -> TermHole
freshTermHole _ = MakeTermHole unit

-- | Direction
data Direction
  = Up
  | Down

derive instance genericDirection :: Generic Direction _

instance showDirection :: Show Direction where
  show x = genericShow x

-- | Ctx
type Ctx
  = { typings :: Map.Map TermVar PolyType
    , kindings :: Map.Map TypeVar Kind
    , instances :: Map.Map TypeVar Type -- monomorphization
    , constructors :: Map.Map TypeVar (List TermVar)
    }
