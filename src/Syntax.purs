module Syntax where

import Data.UUID (UUID)
import Prim hiding (Type)
import Data.List (List)

-- | PolyType
data PolyType
  = PolyType { params :: List TypeVar, typ :: Type }

-- | Type 
data Type
  = NeuType NeuType
  | ArrowType ArrowType
  | HoleType HoleType

type NeuType
  = { typeVar :: TypeVar, args :: List Type }

type ArrowType
  = { dom :: Type, cod :: Type }

type HoleType
  = { typeHole :: TypeHole }

-- | Term
data Term
  = VarTerm VarTerm
  | AppTerm AppTerm
  | LamTerm LamTerm
  | LetTerm LetTerm
  | BufTerm BufTerm
  | DataTerm DataTerm
  | TypeChangeTerm TypeChangeTerm
  | TypeChangeCtxTerm ChangeCtx Term
  | HoleTerm HoleTerm

type VarTerm
  = { termVar :: TermVar }

type AppTerm
  = { apl :: Term, arg :: Term }

type LamTerm
  = { termVar :: TermVar, sig :: Type, bod :: Term }

type LetTerm
  = { termVar :: TermVar, sig :: PolyType, imp :: Term, bod :: Term }

type DataTerm
  = { typeVar :: TypeVar, params :: List Type, constrs :: List Constr, bod :: Term }

type BufTerm
  = { sig :: Type, imp :: Term, bod :: Term }

type TypeChangeTerm
  = { change :: TypeChange, bod :: Term }

type ChangeCtxTerm
  = { changeCtx :: ChangeCtx, bod :: Term }

type HoleTerm
  = { termHole :: TermHole }

-- | Constr
data Constr
  = Constr { termVar :: TermVar, params :: List Type }

-- | TypeChange
type TypeChange
  = { direction :: Direction, pre :: PreTypeChange }

data PreTypeChange
  = ArrowTypeChange ArrowTypeChange
  | ArrowPlusTypeChange ArrowPlusTypeChange
  | ArrowMinusTypeChange ArrowMinusTypeChange
  | NeuTypeChange NeuTypeChange
  | HoleChange HoleChange -- TODO: what does this do?
  | ReplaceTypeChange ReplaceTypeChange

type ArrowTypeChange
  = { dom :: TypeChange, cod :: TypeChange }

type ArrowPlusTypeChange
  = { dom :: Type, cod :: TypeChange }

type ArrowMinusTypeChange
  = { dom :: Type, cod :: TypeChange }

type NeuTypeChange
  = { typeVar :: TypeVar, params :: List ParamChange }

type HoleChange
  = { typeHole :: TypeHole }

type ReplaceTypeChange
  = { old :: Type, new :: Type }

-- | ParamChange
data ParamChange
  = ParamChange TypeChange
  | PlusParamChange Type
  | MinusParamChange Type

-- | ChangeCtx
type ChangeCtx
  = { dir :: Direction, pre :: PreChangeCtx }

data PreChangeCtx
  = TermVarChangeCtx TermVarChangeCtx

type TermVarChangeCtx
  = { termVar :: TermVar, termVarChange :: TermVarChange }

-- | TermVarChange
data TermVarChange
  = TermVarChange TypeChange
  | DeleteVarChange PolyType
  | InsertVarChange PolyType

-- | TypeVar
data TypeVar
  = MakeTypeVar UUID

-- | TermVar 
data TermVar
  = MakeTermVar UUID

-- | TypeHole
data TypeHole
  = MakeTypeHole UUID

-- | TermHole
data TermHole
  = MakeTermHole

-- | Direction
data Direction
  = Up
  | Down
