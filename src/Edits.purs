module Edits where

import Prelude
import Syntax
import Data.Array (concat)
import Data.Lazy (defer)
import Model (Edit)
import Utility (impossible, unimplemented)

type EditBuilder
  = (Term -> Term) -> Term -> Edit

pathLam =
  { bod: \{ dom } bod -> LamTerm { termVar: freshTermVar unit, dom, bod }
  }

pathAppApl apl = case infer apl of
  ArrType { dom } -> AppTerm { apl, arg: freshHoleTerm dom }
  _ -> impossible "pathApp.arg: apl does not have function type"

pathDefBod bod = DefTerm { termVar: freshTermVar unit, sig: monoType alpha, imp: freshHoleTerm alpha, bod }
  where
  alpha = freshHoleType unit

pathDefImp imp = DefTerm { termVar: freshTermVar unit, sig: monoType (infer imp), imp, bod: freshHoleTerm (freshHoleType unit) }

pathBufBod bod = BufTerm { sig: alpha, imp: freshHoleTerm alpha, bod }
  where
  alpha = freshHoleType unit

pathTypeChange { dir, tych } bod = TypeChangeTerm { dir, tych, bod }

-- default direction is Down, i.e. if direcion=Up then the typechange is inverted
type PreTypeChangeEdit
  = { path :: Term -> Term, tych :: Term -> TypeChange }

buildTypeChangeEdit :: _ -> PreTypeChangeEdit -> EditBuilder
buildTypeChangeEdit { label, dir } { path, tych } up tm =
  let
    boundary =
      pathTypeChange
        { dir
        , tych: tych tm
        }
  in
    { label
    , term:
        defer \_ -> case dir of
          Down -> up <<< path <<< boundary $ tm
          Up -> up <<< boundary <<< path $ tm
    }

-- Lam 
preLam _ =
  { path: pathLam.bod { dom: alpha }
  , tych: \bod -> ArrowMinusTypeChange { dom: alpha, bod: identityTypeChange (infer bod) }
  }
  where
  alpha = freshHoleType unit

enLam _ = buildTypeChangeEdit { label: "enLam", dir: Up } (preLam unit)

inLam _ = buildTypeChangeEdit { label: "inLam", dir: Down } (preLam unit)

unLam _ =
  buildTypeChangeEdit { label: "unLam", dir: Down }
    { path: identity
    , tych:
        \tm -> case infer tm of
          ArrType { dom, cod } -> ArrowPlusTypeChange { dom, bod: identityTypeChange cod }
          _ -> impossible "cannot unLam a term with a non-function type"
    }

-- App
preAppApl _ =
  { path: \apl -> pathAppApl apl
  , tych: \apl -> ArrowPlusTypeChange { dom: freshHoleType unit, bod: identityTypeChange (infer apl) }
  }

-- f ~~~> f a
enArg _ = buildTypeChangeEdit { label: "enArg", dir: Up } (preAppApl unit)

inArg _ = buildTypeChangeEdit { label: "inArg", dir: Down } (preAppApl unit)

-- Def
preDefBod _ =
  { path: \bod -> pathDefBod bod
  , tych: \bod -> identityTypeChange (infer bod)
  }

enDefBod _ = buildTypeChangeEdit { label: "enDefBod", dir: Up } (preDefBod unit)

-- dig 
dig _ =
  buildTypeChangeEdit { label: "dig", dir: Down }
    { path: \tm -> freshHoleTerm (infer tm)
    , tych: \tm -> identityTypeChange (infer tm)
    }

-- Edits
editsCommon path tm =
  concat
    [ [ enLam unit path tm
      , inLam unit path tm
      , inArg unit path tm
      , enDefBod unit path tm
      ]
    , case tm of
        HoleTerm _ -> []
        _ -> [ dig unit path tm ]
    , case infer tm of
        ArrType _ ->
          [ enArg unit path tm
          , unLam unit path tm
          ]
        _ -> []
    ]

editsVar path tm = editsCommon path tm

editsApp path tm = editsCommon path tm

editsLam path tm = editsCommon path tm

editsDef path tm = editsCommon path tm

editsBuf path tm = editsCommon path tm

editsDat path tm = editsCommon path tm

editsTypeChange path tm = editsCommon path tm

editsCtxChange path tm = editsCommon path tm

editsHole path tm = editsCommon path tm

editsTop path tm = editsCommon path tm
