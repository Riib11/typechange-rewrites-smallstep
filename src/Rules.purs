module Rules where

import Change
import Prelude
import Rewrite
import Syntax
import Data.Array as Array
import Data.Maybe (Maybe(..))

-- rule rules
rules :: Array Rule
rules =
  Array.concat
    [ rulesInwards
    , rulesOutwards
    ]

-- # Inwards/Down
rulesInwards :: Array Rule
rulesInwards =
  [ case _ of
      -- ## eliminate identity typechange
      TypeChangeTerm { dir: Up, tych: ReplaceTypeChange { old, new }, bod }
        | old == new -> pure $ Just bod
      -- ## requiring a hole to change
      -- ↓{? : α}[α ~[ξ]~> α']  
      -- ~~~>
      -- ? : α'
      TypeChangeTerm { dir: Down, tych, bod: HoleTerm _ } -> do
        pure $ Just
          $ freshHoleTerm (typeChangeCodomain tych)
      -- ## ArrowPlusTypeChange
      -- ↓{b : β}[β ~[+ α -> [ξβ]]~> (α -> β')] 
      -- ~~~>
      -- fun _ : α => ↓{b : β}[β ~[ξβ]~> β']
      TypeChangeTerm { dir: Down, tych: ArrowPlusTypeChange { dom: alpha, bod: tych_beta }, bod: b } -> do
        pure $ Just $ LamTerm { termVar: freshTermVar unit, dom: alpha, bod: TypeChangeTerm { dir: Down, tych: tych_beta, bod: b } }
      -- ## ArrowMinusTypeChange
      -- ↓{f : α -> β}[(α -> β) ~[- α -> [ξβ]]~> β'] 
      -- ~~~>
      -- ↓{f (? : α) : β}[β ~[ξβ]~> β']
      TypeChangeTerm { dir: Down, tych: ArrowMinusTypeChange { dom: alpha, bod: tych_beta }, bod: f } -> do
        pure $ Just $ AppTerm { apl: TypeChangeTerm { dir: Down, tych: ArrTypeChange { dom: identityTypeChange alpha, cod: tych_beta }, bod: f }, arg: freshHoleTerm alpha }
      -- ## ReplaceTypeChange
      -- ↓{a : α}[α ~[α replaced-by α']~> α'] 
      -- ~~~>
      -- buf a : α in ? : α'
      TypeChangeTerm { tych: ReplaceTypeChange { old: alpha, new: alpha' }, bod: a } -> do
        pure $ Just $ BufTerm { sig: alpha, imp: a, bod: freshHoleTerm alpha' }
      -- ## ArrTypeChange
      -- ↓{fun x : α => b : β}[(α -> β) ~[ξα -> ξβ]~> (α' -> β')] 
      -- ~~~>
      -- fun x : α[ξα] => ↓{b : β}[β ~[ξβ]~> β']
      TypeChangeTerm { dir: Down, tych: ArrTypeChange { dom: tych_alpha, cod: tych_beta }, bod: LamTerm { termVar: x, dom: alpha, bod: b } } -> do
        pure $ Just $ LamTerm { termVar: x, dom: changeType tych_alpha alpha, bod: TypeChangeTerm { dir: Down, tych: tych_beta, bod: b } }
      -- ## _
      -- ↓{x : α}[α ~[ξ]~> α'] 
      -- ~~~>
      -- ↑{x : α'}[x : α' ~[ξⁱ]~> α]
      TypeChangeTerm { dir: Down, tych: tych_alpha, bod: VarTerm { termVar: x, argTys: betas } } -> do
        pure $ Just
          $ CtxChangeTerm
              { dir: Up
              , ctxch: TermVarCtxChange { termVar: x, termVarChange: TypeChangeTermVarChange tych_alpha }
              , bod: VarTerm { termVar: x, argTys: betas, ty: typeChangeCodomain tych_alpha }
              }
      -- ↓{a : α}[α ~[ξ]~> α']  
      -- ~~~>
      -- buf a : α in ? : α'
      TypeChangeTerm { dir: Down, tych, bod: a } -> do
        pure $ Just
          $ BufTerm
              { sig: typeChangeDomain tych
              , imp: a
              , bod: freshHoleTerm (typeChangeCodomain tych)
              }
      -- 
      _ -> pure Nothing
  ]

-- # Outwards/Up
rulesOutwards :: Array Rule
rulesOutwards =
  [ case _ of
      -- ## eliminate identity typechange
      TypeChangeTerm { dir: Up, tych: ReplaceTypeChange { old, new }, bod }
        | old == new -> pure $ Just bod
      -- ## ReplaceTypeChange
      -- ## ArrowPlusTypeChange
      -- ↑{b : β}[+ α -> [ch]] (? : α) ~~~>
      -- ↑{b : β}[ch]
      AppTerm { apl: TypeChangeTerm { dir: Up, tych: ArrowPlusTypeChange { dom: alpha, bod: tych }, bod: b }, arg: HoleTerm _ } -> do
        pure $ Just
          $ TypeChangeTerm { dir: Up, tych, bod: b }
      -- ↑{b : β}[+ α -> [ch]] (a : α) ~~~>
      -- buf a : α in ↑{b : β}[ch]
      AppTerm { apl: TypeChangeTerm { dir: Up, tych: ArrowPlusTypeChange { dom: alpha, bod: tych }, bod: b }, arg: a } -> do
        pure $ Just
          $ BufTerm
              { imp: a
              , sig: alpha
              , bod: TypeChangeTerm { dir: Up, tych, bod: b }
              }
      --   ## ArrowMinusTypeChange
      -- -- nothing special
      --   ## ArrTypeChange
      --   ↑{f : α -> β}[(α → β) ~[ξα -> ξβ]~> (α' → β')] a' ~~~>
      --   ↑{ (f : α -> β) ↓{a'}[α' ~[ξα]~> α] }[β ~[ξβ]~> β']
      AppTerm { apl: TypeChangeTerm { dir: Up, tych: ArrTypeChange { dom: tych_alpha, cod: tych_beta }, bod: f }, arg: a' } -> do
        pure $ Just $ TypeChangeTerm { dir: Up, tych: tych_beta, bod: AppTerm { apl: f, arg: TypeChangeTerm { dir: Down, tych: tych_alpha, bod: a' } } }
      -- ## propogate upwards
      -- fun x : alpha => ↑{b : beta}[ch] ~~~>
      -- ↑{fun x : alpha => b : beta}[alpha -> ch]
      LamTerm { termVar: x, dom: alpha, bod: TypeChangeTerm { dir: Up, tych: tych_beta, bod: b } } -> do
        pure $ Just $ TypeChangeTerm { dir: Up, tych: codomainTypeChange alpha tych_beta, bod: LamTerm { termVar: x, dom: alpha, bod: b } }
      -- (f : α -> β) ↑{a' : α'}[ α' ~[ξ]~> α ] ~~~>
      -- ↓{f : α -> β}[ (α -> β) ~[ ξⁱ -> β ]~> (α' -> β) ] (a' : α')
      app@(AppTerm { apl: f, arg: TypeChangeTerm { dir: Up, tych, bod: a' } }) -> do
        let
          beta = infer app
        pure $ Just
          $ AppTerm
              { apl: TypeChangeTerm { dir: Down, tych: domainTypeChange (invertTypeChange tych) beta, bod: f }
              , arg: a'
              }
      -- let x : α' = ↑{a}[α ~[ξ]~> α'] in b ~~~>
      -- let x : ↓{α'}[α' ~[ξⁱ]~> α] = a in ↓{b}[x : α' ~[ξⁱ]~> α]
      DefTerm { termVar: x, sig: alpha, imp: TypeChangeTerm { dir: Up, tych, bod: a }, bod: b } -> do
        pure $ Just
          $ DefTerm
              { termVar: x
              , sig: changePolyType (invertTypeChange tych) alpha
              , imp: a
              , bod: CtxChangeTerm { dir: Down, ctxch: TermVarCtxChange { termVar: x, termVarChange: TypeChangeTermVarChange (invertTypeChange tych) }, bod: b }
              }
      -- buf ↑{a}[α ~[ξ]~> α'] : α' in b ~~~>
      -- buf a : ↓{α'}[α' ~[ξⁱ]~> α] in b
      BufTerm { imp: TypeChangeTerm { dir: Up, tych, bod: a }, sig: alpha, bod: b } -> do
        pure $ Just
          $ BufTerm
              { imp: a
              , sig: changeType (invertTypeChange tych) alpha
              , bod: b
              }
      -- let x : alpha = a in ↑{b : beta}[ch] ~~~> ↑{let x : alpha = a in b : beta}[ch]
      DefTerm def@{ bod: TypeChangeTerm { dir: Up, tych, bod: b } } -> pure $ Just $ TypeChangeTerm { dir: Up, tych, bod: DefTerm def { bod = b } }
      -- buf a : alpha     in ↑{b : beta}[ch] ~~~> ↑{buf a : alpha     in b : beta}[ch]
      BufTerm buf@{ bod: TypeChangeTerm { dir: Up, tych, bod: b } } -> pure $ Just $ TypeChangeTerm { dir: Up, tych, bod: BufTerm buf { bod = b } }
      -- data D = constrs  in ↑{b : beta}[ch] ~~~> ↑{data D = constrs  in b : beta}[ch]
      DatTerm dat@{ bod: TypeChangeTerm { dir: Up, tych, bod: b } } -> pure $ Just $ TypeChangeTerm { dir: Up, tych, bod: DatTerm dat { bod = b } }
      -- ↑{a : alpha}[ξ] : alpha' ~~~> a : ↓{alpha'}[ξⁱ]
      TopTerm { bod: TypeChangeTerm { dir: Up, tych, bod: a }, sig: alpha } -> pure $ Just $ TopTerm { bod: a, sig: changeType (invertTypeChange tych) alpha }
      -- 
      _ -> pure Nothing
  ]
