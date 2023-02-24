module RenderTerm where

import Model
import Prelude
import Edits
import Prim hiding (Type)
import Syntax
import Data.Array as Array
import Data.Lazy (Lazy, defer)
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
term ∷ forall w. (Term -> Term) -> Term → HH.HTML w Action
term path tm@(VarTerm var) =
  HH.div
    [ HP.class_ (HH.ClassName "node VarTerm")
    , HE.onClick \evt -> SetEditsAction evt $ editsVar path tm
    ]
    [ HH.div [ HP.class_ (HH.ClassName "VarTerm-termVar") ] [ termVar var.termVar ]
    , angles [ HP.class_ (HH.ClassName "VarTerm-argTys") ]
        (typ <$> List.toUnfoldable var.argTys)
    ]

term path tm@(AppTerm app) =
  parens
    [ HP.class_ (HH.ClassName "node AppTerm")
    , HE.onClick \evt -> SetEditsAction evt $ editsApp path tm
    ]
    [ HH.div [ HP.class_ (HH.ClassName "AppTerm-apl") ] [ term (path <<< AppTerm <<< app { apl = _ }) app.apl ]
    , space
    , HH.div [ HP.class_ (HH.ClassName "AppTerm-arg") ] [ term (path <<< AppTerm <<< app { arg = _ }) app.arg ]
    ]

term path tm@(LamTerm lam) =
  parens
    [ HP.class_ (HH.ClassName "node LamTerm")
    , HE.onClick \evt -> SetEditsAction evt $ editsLam path tm
    ]
    [ lambda
    , HH.div [ HP.class_ (HH.ClassName "LamTerm-termVar") ] [ termVar lam.termVar ]
    , colon
    , HH.div [ HP.class_ (HH.ClassName "LamTerm-dom") ] [ typ lam.dom ]
    , mapsto
    , HH.div [ HP.class_ (HH.ClassName "LamTerm-bod") ] [ term (path <<< LamTerm <<< lam { bod = _ }) lam.bod ]
    ]

term path tm@(DefTerm def) =
  parens
    [ HP.class_ (HH.ClassName "node DefTerm")
    , HE.onClick \evt -> SetEditsAction evt $ editsDef path tm
    ]
    [ let_
    , HH.div [ HP.class_ (HH.ClassName "DefTerm-termVar") ] [ termVar def.termVar ]
    , colon
    , HH.div [ HP.class_ (HH.ClassName "DefTerm-sig") ] [ polyType def.sig ]
    , equal
    , HH.div [ HP.class_ (HH.ClassName "DefTerm-imp") ] [ term (path <<< DefTerm <<< def { imp = _ }) def.imp ]
    , in_
    , HH.div [ HP.class_ (HH.ClassName "DefTerm-bod") ] [ term (path <<< DefTerm <<< def { bod = _ }) def.bod ]
    ]

term path tm@(BufTerm buf) =
  parens
    [ HP.class_ (HH.ClassName "node BufTerm")
    , HE.onClick \evt -> SetEditsAction evt $ editsBuf path tm
    ]
    [ buf_
    , HH.div
        [ HP.class_ (HH.ClassName "BufTerm-imp") ]
        [ term (path <<< BufTerm <<< buf { imp = _ }) buf.imp ]
    , colon
    , HH.div
        [ HP.class_ (HH.ClassName "BufTerm-sig") ]
        [ typ buf.sig ]
    , in_
    , HH.div
        [ HP.class_ (HH.ClassName "BufTerm-bod") ]
        [ term (path <<< BufTerm <<< buf { bod = _ }) buf.bod ]
    ]

term path tm@(DatTerm dat) =
  parens
    [ HP.class_ (HH.ClassName "node DatTerm")
    , HE.onClick \evt -> SetEditsAction evt $ editsDat path tm
    ]
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
        [ HP.class_ (HH.ClassName "DatTerm-bod") ]
        [ term (path <<< DatTerm <<< dat { bod = _ }) dat.bod ]
    ]

term path tm@(TopTerm top) =
  HH.div
    [ HP.class_ (HH.ClassName "node TopTerm")
    , HE.onClick \evt -> SetEditsAction evt $ editsTop path tm
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "TopTerm-bod") ]
        [ term (path <<< TopTerm <<< top { bod = _ }) top.bod ]
    , colon
    , HH.div
        [ HP.class_ (HH.ClassName "TopTerm-sig") ]
        [ typ top.sig ]
    ]

term path tm@(TypeChangeTerm tych) =
  HH.div
    [ HP.class_ (HH.ClassName "node TypeChangeTerm")
    , HE.onClick \evt -> SetEditsAction evt $ editsTypeChange path tm
    ]
    [ case tych.dir of
        Down -> downarrow
        Up -> uparrow
    , HH.div
        [ HP.class_ (HH.ClassName "TypeChangeTerm-bod") ]
        [ term (path <<< TypeChangeTerm <<< tych { bod = _ }) tych.bod ]
    , HH.div
        [ HP.class_ (HH.ClassName "TypeChangeTerm-tych") ]
        [ typeChange tych.tych ]
    ]

term path tm@(CtxChangeTerm ctxch) =
  HH.div
    [ HP.class_ (HH.ClassName "node CtxChangeTerm")
    , HE.onClick \evt -> SetEditsAction evt $ editsCtxChange path tm
    ]
    [ case ctxch.dir of
        Down -> downarrow
        Up -> uparrow
    , HH.div
        [ HP.class_ (HH.ClassName "CtxChangeTerm-ctxch") ]
        [ ctxChange ctxch.ctxch ]
    , HH.div
        [ HP.class_ (HH.ClassName "CtxChangeTerm-bod") ]
        [ term (path <<< CtxChangeTerm <<< ctxch { bod = _ }) ctxch.bod ]
    ]

term path tm@(HoleTerm hole) =
  HH.div
    [ HP.class_ (HH.ClassName "node HoleTerm")
    , HE.onClick \evt -> SetEditsAction evt $ editsHole path tm
    ]
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
    [ HP.class_ (HH.ClassName "node PolyType") ]
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
      [ HP.class_ (HH.ClassName "node NeuType") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "NeuType-typeVar") ]
          [ typeVar neu.typeVar ]
      , angles
          [ HP.class_ (HH.ClassName "NeuType-args") ]
          $ commas (typ <$> List.toUnfoldable neu.args)
      ]
  ArrType arr ->
    parens
      [ HP.class_ (HH.ClassName "node ArrType") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "ArrType-dom") ]
          [ typ arr.dom ]
      , arrow
      , HH.div
          [ HP.class_ (HH.ClassName "ArrType-cod") ]
          [ typ arr.cod ]
      ]
  HoleType hole ->
    HH.div
      [ HP.class_ (HH.ClassName "node HoleType") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "HoleType-typeHole") ]
          [ typeHole hole.typeHole ]
      ]

constr (Constr constr) =
  HH.div
    [ HP.class_ (HH.ClassName "node Constr") ]
    [ HH.div
        [ HP.class_ (HH.ClassName "Constr-termVar") ]
        [ termVar constr.termVar ]
    , parens
        [ HP.class_ (HH.ClassName "Constr-ty") ]
        [ typ constr.ty ]
    ]

typeChange = case _ of
  ArrTypeChange arr ->
    parens
      [ HP.class_ (HH.ClassName "node ArrTypeChange") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "ArrTypeChange-dom") ]
          [ typeChange arr.dom ]
      , arrow
      , HH.div
          [ HP.class_ (HH.ClassName "ArrTypeChange-cod") ]
          [ typeChange arr.cod ]
      ]
  ReplaceTypeChange rep
    | rep.old == rep.new ->
      HH.div [ HP.class_ (HH.ClassName "node IdentityTypeChange") ]
        [ HH.div [ HP.class_ (HH.ClassName "IdentityTypeChange") ] [ typ rep.new ] ]
    | otherwise ->
      parens
        [ HP.class_ (HH.ClassName "node ReplaceTypeChange") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "ReplaceTypeChange-old") ]
            [ typ rep.old ]
        , squigarrow
        , HH.div
            [ HP.class_ (HH.ClassName "ReplaceTypeChange-new") ]
            [ typ rep.new ]
        ]
  ArrowPlusTypeChange plus ->
    parens
      [ HP.class_ (HH.ClassName "node ArrowPlusTypeChange") ]
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
    parens
      [ HP.class_ (HH.ClassName "node ArrowMinusTypeChange") ]
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
      brackets
        [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TypeChangeTermVarChange" ]) ]
        [ HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TermVarCtxChange-termVar" ]) ]
            [ termVar tmvarctxch.termVar ]
        , colon
        , HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TypeChangeTermVarChange-tych" ]) ]
            [ typeChange tych ]
        ]
    DeleteVarChange pty ->
      brackets
        [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TypeChangeTermVarChange" ]) ]
        [ HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TermVarCtxChange-termVar" ]) ]
            [ termVar tmvarctxch.termVar ]
        , colon
        , minus_
        , HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "DeleteVarChange-polyType" ]) ]
            [ polyType pty ]
        ]
    InsertVarChange pty ->
      brackets
        [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TypeChangeTermVarChange" ]) ]
        [ HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "TermVarCtxChange-termVar" ]) ]
            [ termVar tmvarctxch.termVar ]
        , colon
        , plus_
        , HH.div
            [ HP.classes (map HH.ClassName [ "TermVarCtxChange", "InsertVarChange-polyType" ]) ]
            [ polyType pty ]
        ]

typeVar tyVar = HH.div [ HP.class_ (HH.ClassName "node TypeVar") ] [ HH.text (show tyVar) ]

termVar tmVar = HH.div [ HP.class_ (HH.ClassName "node TermVar") ] [ HH.text (show tmVar) ]

termHole tmHole = HH.div [ HP.class_ (HH.ClassName "node TermHole") ] [ HH.text "?" ]

typeHole tyHole = HH.div [ HP.class_ (HH.ClassName "node TypeHole") ] [ HH.text (show tyHole) ]

angles props hs = HH.div props $ [ langle ] <> hs <> [ rangle ]

braces props hs = HH.div props $ [ lbrace ] <> hs <> [ rbrace ]

-- parens props hs = HH.div props $ [ lparen ] <> hs <> [ rparen ]
parens props hs =
  HH.div
    [ HP.class_ (HH.ClassName "parens") ]
    [ HH.div props hs ]

-- brackets props hs = HH.div props $ [ lbrak ] <> hs <> [ rbrak ]
brackets props hs =
  HH.div
    [ HP.class_ (HH.ClassName "brackets") ]
    [ HH.div props hs ]

commas hs = Array.intercalate [ comma ] (pure <$> hs)

verticals hs = Array.intercalate [ vertical ] (pure <$> hs)

lbrak = makePunctuation "lbrak" "["

rbrak = makePunctuation "rbrak" "]"

lparen = makePunctuation "lparen" "("

rparen = makePunctuation "rparen" ")"

langle = makePunctuation "langle" "⟨"

rangle = makePunctuation "rangle" "⟩"

lbrace = makePunctuation "lbrace" "{"

rbrace = makePunctuation "rbrace" "}"

comma = makePunctuation "comma" ","

vertical = makePunctuation "vertical" "|"

space = makePunctuation "space" " "

lambda = makePunctuation "lambda" "λ"

colon = makePunctuation "colon" ":"

mapsto = makePunctuation "mapsto" "⇒"

arrow = makePunctuation "arrow" "→"

squigarrow = makePunctuation "squigarrow" "⟿"

let_ = makePunctuation "let" "let"

in_ = makePunctuation "in" "in"

buf_ = makePunctuation "buf" "buf"

data_ = makePunctuation "data" "data"

forall_ = makePunctuation "forall" "∀"

equal = makePunctuation "equal" "="

period = makePunctuation "period" "."

plus_ = makePunctuation "plus" "+"

minus_ = makePunctuation "minus" "-"

uparrow = makePunctuation "uparrow" "↑"

downarrow = makePunctuation "downarrow" "↓"

makePunctuation name symbol =
  HH.div
    [ HP.class_ (HH.ClassName ("punctuation punctuation-" <> name)) ]
    [ HH.text symbol ]
