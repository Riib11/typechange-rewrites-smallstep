module RenderTerm where

import Prelude
import Prim hiding (Type)
import Syntax
import Data.Array as Array
import Data.List as List
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Utility (unimplemented)

-- w = (H.ComponentSlot slots m action)
term (VarTerm var) =
  HH.div [ HP.class_ (HH.ClassName "VarTerm") ]
    [ HH.div [ HP.class_ (HH.ClassName "VarTerm-termVar") ] [ termVar var.termVar ]
    , angles [ HP.class_ (HH.ClassName "VarTerm-argTys") ]
        (typ <$> List.toUnfoldable var.argTys)
    ]

term (AppTerm app) =
  HH.div [ HP.class_ (HH.ClassName "AppTerm") ]
    [ HH.div [ HP.class_ (HH.ClassName "AppTerm-apl") ] [ term app.apl ]
    , space
    , HH.div [ HP.class_ (HH.ClassName "AppTerm-arg") ] [ term app.arg ]
    ]

term (LamTerm lam) =
  HH.div [ HP.class_ (HH.ClassName "LamTerm") ]
    [ lambda
    , HH.div [ HP.class_ (HH.ClassName "LamTerm-termVar") ] [ termVar lam.termVar ]
    , colon
    , HH.div [ HP.class_ (HH.ClassName "LamTerm-dom") ] [ typ lam.dom ]
    , mapsto
    , HH.div [ HP.class_ (HH.ClassName "LamTerm-bod") ] [ term lam.bod ]
    ]

term (DefTerm def) =
  HH.div [ HP.class_ (HH.ClassName "DefTerm") ]
    [ let_
    , HH.div [ HP.class_ (HH.ClassName "DefTerm-termVar") ] [ termVar def.termVar ]
    , colon
    , HH.div [ HP.class_ (HH.ClassName "DefTerm-sig") ] [ polyType def.sig ]
    , equal
    , HH.div [ HP.class_ (HH.ClassName "DefTerm-imp") ] [ term def.imp ]
    , in_
    , HH.div [ HP.class_ (HH.ClassName "DefTerm-bod") ] [ term def.bod ]
    ]

term (BufTerm buf) =
  HH.div
    [ HP.class_ (HH.ClassName "BufTerm") ]
    [ buf_
    , HH.div
        [ HP.class_ (HH.ClassName "BufTerm-imp") ]
        [ term buf.imp ]
    , colon
    , HH.div
        [ HP.class_ (HH.ClassName "BufTerm-sig") ]
        [ typ buf.sig ]
    , in_
    , HH.div
        [ HP.class_ (HH.ClassName "BufTerm-bod") ]
        [ term buf.bod ]
    ]

term (DatTerm dat) =
  HH.div
    [ HP.class_ (HH.ClassName "DatTerm") ]
    [ data_
    , HH.div
        [ HP.class_ (HH.ClassName "DatTerm-typeVar") ]
        [ typeVar dat.typeVar ]
    , angles
        [ HP.class_ (HH.ClassName "DatTerm-params") ]
        $ commas (HH.text <<< show <$> List.toUnfoldable dat.params)
    , equal
    , braces
        [ HP.class_ (HH.ClassName "DatTerm-constrs") ]
        $ verticals (constr <$> List.toUnfoldable dat.constrs)
    , in_
    , HH.div
        [ HP.class_ (HH.ClassName "datTerm-bod") ]
        [ term dat.bod ]
    ]

term (TypeChangeTerm tych) =
  HH.div
    [ HP.class_ (HH.ClassName "TypeChangeTerm") ]
    [ HH.div
        [ HP.class_ (HH.ClassName "TypeChangeTerm-tych") ]
        [ typeChange tych.tych ]
    , braces
        [ HP.class_ (HH.ClassName "TypeChangeTerm-bod") ]
        [ term tych.bod ]
    ]

term (CtxChangeTerm ctxch) =
  HH.div
    [ HP.class_ (HH.ClassName "CtxChangeTerm") ]
    [ HH.div
        [ HP.class_ (HH.ClassName "CtxChangeTerm-ctxch") ]
        [ ctxChange ctxch.ctxch ]
    , braces
        [ HP.class_ (HH.ClassName "CtxChangeTerm-bod") ]
        [ term ctxch.bod ]
    ]

term (HoleTerm hole) =
  HH.div
    [ HP.class_ (HH.ClassName "HoleTerm") ]
    [ HH.div
        [ HP.class_ (HH.ClassName "HoleTerm-hole") ]
        [ termHole hole.termHole ]
    , colon
    , HH.div
        [ HP.class_ (HH.ClassName "HoleTerm-ty") ]
        [ typ hole.ty ]
    ]

polyType (PolyType pty) =
  HH.div
    [ HP.class_ (HH.ClassName "PolyType") ]
    [ forall_
    , HH.div
        [ HP.class_ (HH.ClassName "PolyType-params") ]
        $ commas (typeVar <$> List.toUnfoldable pty.params)
    , period
    , HH.div
        [ HP.class_ (HH.ClassName "PolyType-ty") ]
        [ typ pty.ty ]
    ]

typ = case _ of
  NeuType neu ->
    HH.div
      [ HP.class_ (HH.ClassName "NeuType") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "NeuType-typeVar") ]
          [ typeVar neu.typeVar ]
      , angles
          [ HP.class_ (HH.ClassName "NeuType-args") ]
          $ commas (typ <$> List.toUnfoldable neu.args)
      ]
  ArrType arr ->
    HH.div
      [ HP.class_ (HH.ClassName "ArrType") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "ArrType-dom") ]
          [ typ arr.dom ]
      , HH.div
          [ HP.class_ (HH.ClassName "ArrType-cod") ]
          [ typ arr.cod ]
      ]
  HoleType hole ->
    HH.div
      [ HP.class_ (HH.ClassName "HoleType") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "HoleType-typeHole") ]
          [ typeHole hole.typeHole ]
      ]

constr (Constr constr) =
  HH.div
    [ HP.class_ (HH.ClassName "Constr") ]
    [ HH.div
        [ HP.class_ (HH.ClassName "Constr-termVar") ]
        [ termVar constr.termVar ]
    , parens
        [ HP.class_ (HH.ClassName "Constr-ty") ]
        [ typ constr.ty ]
    ]

typeChange = case _ of
  ArrTypeChange arr ->
    HH.div
      [ HP.class_ (HH.ClassName "ArrTypeChange") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "ArrTypeChange-dom") ]
          [ typeChange arr.dom ]
      , arrow
      , HH.div
          [ HP.class_ (HH.ClassName "ArrTypeChange-cod") ]
          [ typeChange arr.cod ]
      ]
  ReplaceTypeChange rep ->
    HH.div
      [ HP.class_ (HH.ClassName "ReplaceTypeChange") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "ReplaceTypeChange-old") ]
          [ typ rep.old ]
      , HH.div
          [ HP.class_ (HH.ClassName "ReplaceTypeChange-new") ]
          [ typ rep.new ]
      ]
  ArrowPlusTypeChange plus ->
    HH.div
      [ HP.class_ (HH.ClassName "ArrowPlusTypeChange") ]
      [ plus_
      , HH.div
          [ HP.class_ (HH.ClassName "ArrowPlusTypeChange-dom") ]
          [ typ plus.dom ]
      , arrow
      , brackets
          [ HP.class_ (HH.ClassName "ArrowPlusTypeChange-bod") ]
          [ typeChange plus.bod ]
      ]
  ArrowMinusTypeChange minus ->
    HH.div
      [ HP.class_ (HH.ClassName "ArrowMinusTypeChange") ]
      [ minus_
      , HH.div
          [ HP.class_ (HH.ClassName "ArrowMinusTypeChange-dom") ]
          [ typ minus.dom ]
      , arrow
      , brackets
          [ HP.class_ (HH.ClassName "ArrowMinusTypeChange-bod") ]
          [ typeChange minus.bod ]
      ]

ctxChange = case _ of
  TermVarCtxChange tmvarctxch -> case tmvarctxch.termVarChange of
    TypeChangeTermVarChange tych ->
      HH.div
        [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TypeChangeTermVarChange" ]) ]
        [ HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TermVarCtxChange-termVar" ]) ]
            [ termVar tmvarctxch.termVar ]
        , HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TypeChangeTermVarChange-tych" ]) ]
            [ typeChange tych ]
        ]
    DeleteVarChange pty ->
      HH.div
        [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TypeChangeTermVarChange" ]) ]
        [ HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TermVarCtxChange-termVar" ]) ]
            [ termVar tmvarctxch.termVar ]
        , HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "DeleteVarChange-polyType" ]) ]
            [ polyType pty ]
        ]
    InsertVarChange pty ->
      HH.div
        [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TypeChangeTermVarChange" ]) ]
        [ HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TermVarCtxChange-termVar" ]) ]
            [ termVar tmvarctxch.termVar ]
        , HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "InsertVarChange-polyType" ]) ]
            [ polyType pty ]
        ]

typeVar tyVar = HH.div [ HP.class_ (HH.ClassName "TypeVar") ] [ HH.text (show tyVar) ]

termVar tmVar = HH.div [ HP.class_ (HH.ClassName "TermVar") ] [ HH.text (show tmVar) ]

termHole tmHole = HH.div [ HP.class_ (HH.ClassName "TermHole") ] [ HH.text "?" ]

typeHole tyHole = HH.div [ HP.class_ (HH.ClassName "TypeHole") ] [ HH.text (show tyHole) ]

angles props hs = HH.div props $ [ langle ] <> hs <> [ rangle ]

braces props hs = HH.div props $ [ lbrace ] <> hs <> [ rbrace ]

parens props hs = HH.div props $ [ lparen ] <> hs <> [ rparen ]

brackets props hs = HH.div props $ [ lbrak ] <> hs <> [ rbrak ]

commas hs = Array.intercalate [ comma ] (pure <$> hs)

verticals hs = Array.intercalate [ vertical ] (pure <$> hs)

lbrak = makePunctuation "lbrak" "["

rbrak = makePunctuation "rbrak" "]"

lparen = makePunctuation "lparen" "("

rparen = makePunctuation "rparen" ")"

langle = makePunctuation "langle" "⟨"

rangle = makePunctuation "rangle" "⟩"

lbrace = makePunctuation "lbrace" "⟨"

rbrace = makePunctuation "rbrace" "⟩"

comma = makePunctuation "comma" ","

vertical = makePunctuation "vertical" "|"

space = makePunctuation "space" " "

lambda = makePunctuation "lambda" "λ"

colon = makePunctuation "colon" ":"

mapsto = makePunctuation "mapsto" "⇒"

arrow = makePunctuation "arrow" "→"

let_ = makePunctuation "let" "let"

in_ = makePunctuation "in" "in"

buf_ = makePunctuation "buf" "buf"

data_ = makePunctuation "data" "data"

forall_ = makePunctuation "forall" "∀"

equal = makePunctuation "equal" "="

period = makePunctuation "period" "."

plus_ = makePunctuation "plus" "+"

minus_ = makePunctuation "minus" "-"

makePunctuation name symbol =
  HH.div
    [ HP.class_ (HH.ClassName ("punctuation punctuation-" <> name)) ]
    [ HH.text symbol ]
