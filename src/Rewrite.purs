module Rewrite where

import Prelude
import Syntax
import Change (changePolyType, changeType)
import Control.Monad.Except (ExceptT)
import Data.Array (foldM, reverse, singleton, uncons, (:))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Fixpoint (fixpointM)
import Utility (arrayuncurry1, arrayuncurry2)

-- TODO: maybe add effects later?
type RewriteM a
  = Identity a

-- | Rule: the type of rewrite rules over terms
type Rule
  = Term -> RewriteM (Maybe Term)

-- | applyRule: Just if the rule was applied somewhere; Nothing if the rule was
-- | not applied anywhere
applyRule :: Rule -> Term -> RewriteM (Maybe Term)
applyRule rule tm =
  rule tm
    >>= case _ of
        Just tm' -> pure $ Just tm'
        Nothing -> case tm of
          VarTerm _var -> pure $ Nothing
          AppTerm app -> helper (arrayuncurry2 \apl arg -> AppTerm app { apl = apl, arg = arg }) [ app.apl, app.arg ]
          LamTerm lam -> helper (arrayuncurry1 \bod -> LamTerm lam { bod = bod }) [ lam.bod ]
          DefTerm def -> helper (arrayuncurry2 \imp bod -> DefTerm def { imp = imp, bod = bod }) [ def.imp, def.bod ]
          BufTerm buf -> helper (arrayuncurry2 \imp bod -> BufTerm buf { imp = imp, bod = bod }) [ buf.imp, buf.bod ]
          DatTerm dat -> helper (arrayuncurry1 \bod -> DatTerm dat { bod = bod }) [ dat.bod ]
          TypeChangeTerm tych -> helper (arrayuncurry1 \bod -> TypeChangeTerm tych { bod = bod }) [ tych.bod ]
          CtxChangeTerm ctxch -> helper (arrayuncurry1 \bod -> CtxChangeTerm ctxch { bod = bod }) [ ctxch.bod ]
          HoleTerm _hole -> pure $ Nothing
  where
  helper :: (Array Term -> Term) -> Array Term -> RewriteM (Maybe Term)
  helper f = go []
    where
    go :: Array Term -> Array Term -> RewriteM (Maybe Term)
    go as' as_ = case uncons as_ of
      Nothing -> pure $ Nothing
      Just { head: a, tail: as } ->
        applyRule rule a
          >>= case _ of
              Just a' -> pure $ Just $ f $ reverse as' <> singleton a' <> as
              Nothing -> go (a : as') as

-- rule system
system :: Array Rule
system =
  [ case _ of
      {- 
        # Inwards/Down
      -}
      {-
        ## ReplaceTypeChange
      -}
      {-
        ↓{a : α}[α ~[α replaced-by α']~> α'] 
        ~~~>
        buf a : α in ? : α'
      -}
      TypeChangeTerm { tych: ReplaceTypeChange { old: alpha, new: alpha' }, bod: a } -> do
        pure $ Just $ BufTerm { sig: alpha, imp: a, bod: freshHoleTerm alpha' }
      {-
        ## ArrowPlusTypeChange
      -}
      {-
        ↓{b : β}[β ~[+ α -> [ξβ]]~> (α -> β')] 
        ~~~>
        fun _ : α => ↓{b : β}[β ~[ξβ]~> β']
      -}
      TypeChangeTerm { dir: Down, tych: ArrowPlusTypeChange { dom: alpha, bod: tych_beta }, bod: b } -> do
        pure $ Just $ LamTerm { termVar: freshTermVar unit, dom: alpha, bod: TypeChangeTerm { dir: Down, tych: tych_beta, bod: b } }
      {-
        ## ArrowMinusTypeChange
      -}
      {-
        ↓{f : α -> β}[(α -> β) ~[- α -> [ξβ]]~> β'] 
        ~~~>
        ↓{f (? : α) : β}[β ~[ξβ]~> β']
      -}
      TypeChangeTerm { dir: Down, tych: ArrowMinusTypeChange { dom: alpha, bod: tych_beta }, bod: f } -> do
        pure $ Just $ AppTerm { apl: TypeChangeTerm { dir: Down, tych: ArrTypeChange { dom: identityTypeChange alpha, cod: tych_beta }, bod: f }, arg: freshHoleTerm alpha, ty: alpha }
      {-
        ## ArrTypeChange
      -}
      {-
        ↓{fun x : α => b : β}[(α -> β) ~[ξα -> ξβ]~> (α' -> β')] 
        ~~~>
        fun x : α[ξα] => ↓{b : β}[β ~[ξβ]~> β']
      -}
      TypeChangeTerm { dir: Down, tych: ArrTypeChange { dom: tych_alpha, cod: tych_beta }, bod: LamTerm { termVar: x, dom: alpha, bod: b } } -> do
        pure $ Just $ LamTerm { termVar: x, dom: changeType tych_alpha alpha, bod: TypeChangeTerm { dir: Down, tych: tych_beta, bod: b } }
      {-
        ## _
      -}
      {-
        ↓{x : α}[α ~[ξ]~> α'] 
        ~~~>
        ↑{x : α'}[x : α' ~[ξⁱ]~> α]
      -}
      TypeChangeTerm { dir: Down, tych: tych_alpha, bod: VarTerm { termVar: x, argTys: betas } } -> do
        pure $ Just
          $ CtxChangeTerm
              { dir: Up
              , ctxch: TermVarCtxChange { termVar: x, termVarChange: TypeChangeTermVarChange tych_alpha }
              , bod: VarTerm { termVar: x, argTys: betas, ty: typeChangeCodomain tych_alpha }
              }
      {-
        ↓{a : α}[α ~[ξ]~> α']  
        ~~~>
        buf a : α in ? : α'
      -}
      TypeChangeTerm { dir: Down, tych, bod: a } -> do
        pure $ Just
          $ BufTerm
              { sig: typeChangeDomain tych
              , imp: a
              , bod: freshHoleTerm (typeChangeCodomain tych)
              }
      {- 
        # Outwards/Up
      -}
      {-
        ## ReplaceTypeChange
      -}
      {-
        ## ArrowPlusTypeChange
      -}
      {-
        ↑{b : β}[+ α -> [ch]] (a : α) ~~~>
        buf a : α in ↑{b : β}[ch]
      -}
      AppTerm { apl: TypeChangeTerm { dir: Up, tych: ArrowPlusTypeChange { dom: alpha, bod: tych }, bod: b }, arg: a } -> do
        pure $ Just
          $ BufTerm
              { imp: a
              , sig: alpha
              , bod: TypeChangeTerm { dir: Up, tych, bod: b }
              }
      {-
        ## ArrowMinusTypeChange
      -}
      -- nothing special
      {-
        ## ArrTypeChange
      -}
      {-
        ↑{f : α -> β}[(α → β) ~[ξα -> ξβ]~> (α' → β')] a' ~~~>
        ↑{ (f : α -> β) ↓{a'}[α' ~[ξα]~> α] }[β ~[ξβ]~> β']
      -}
      AppTerm { apl: TypeChangeTerm { dir: Up, tych: ArrTypeChange { dom: tych_alpha, cod: tych_beta }, bod: f }, arg: a' } -> do
        pure $ Just $ TypeChangeTerm { dir: Up, tych: tych_beta, bod: AppTerm { apl: f, arg: TypeChangeTerm { dir: Down, tych: tych_alpha, bod: a' }, ty: typeChangeCodomain tych_beta } }
      {-
        ## propogate upwards
      -}
      {-
        fun x : alpha => ↑{b : beta}[ch] ~~~>
        ↑{fun x : alpha => b : beta}[alpha -> ch]
      -}
      LamTerm { termVar: x, dom: alpha, bod: TypeChangeTerm { dir: Up, tych: tych_beta, bod: b } } -> do
        pure $ Just $ TypeChangeTerm { dir: Up, tych: codomainTypeChange alpha tych_beta, bod: LamTerm { termVar: x, dom: alpha, bod: b } }
      {-
        (f : α -> β) ↑{a' : α'}[ α' ~[ξ]~> α ] ~~~>
        ↓{f : α -> β}[ (α -> β) ~[ ξⁱ -> β ]~> (α' -> β) ] (a' : α')
      -}
      app@(AppTerm { apl: f, arg: TypeChangeTerm { dir: Up, tych, bod: a' } }) -> do
        let
          beta = infer app
        pure $ Just
          $ AppTerm
              { apl: TypeChangeTerm { dir: Down, tych: domainTypeChange (invertTypeChange tych) beta, bod: f }
              , arg: a'
              , ty: beta
              }
      {-
        let x : α' = ↑{a}[α ~[ξ]~> α'] in b ~~~>
        let x : ↓{α'}[α' ~[ξⁱ]~> α] = a in ↓{b}[x : α' ~[ξⁱ]~> α]
      -}
      DefTerm { termVar: x, sig: alpha, imp: TypeChangeTerm { dir: Up, tych, bod: a }, bod: b } -> do
        pure $ Just
          $ DefTerm
              { termVar: x
              , sig: changePolyType (invertTypeChange tych) alpha
              , imp: a
              , bod: CtxChangeTerm { dir: Down, ctxch: TermVarCtxChange { termVar: x, termVarChange: TypeChangeTermVarChange (invertTypeChange tych) }, bod: b }
              }
      {-
        let x : alpha = a in ↑{b : beta}[ch] ~~~> ↑{let x : alpha = a in b : beta}[ch]
        buf a : alpha     in ↑{b : beta}[ch] ~~~> ↑{buf a : alpha     in b : beta}[ch]
        data D = constrs  in ↑{b : beta}[ch] ~~~> ↑{data D = constrs  in b : beta}[ch]
      -}
      DefTerm def@{ bod: TypeChangeTerm { dir: Up, tych, bod: b } } -> pure $ Just $ TypeChangeTerm { dir: Up, tych, bod: DefTerm def { bod = b } }
      BufTerm buf@{ bod: TypeChangeTerm { dir: Up, tych, bod: b } } -> pure $ Just $ TypeChangeTerm { dir: Up, tych, bod: BufTerm buf { bod = b } }
      DatTerm dat@{ bod: TypeChangeTerm { dir: Up, tych, bod: b } } -> pure $ Just $ TypeChangeTerm { dir: Up, tych, bod: DatTerm dat { bod = b } }
      {-
        otherwise, no rewrite
      -}
      _ -> pure Nothing
  ]

fixpointSystem :: Term -> RewriteM Term
fixpointSystem =
  fixpointM
    ( foldM
        ( \m_mb_tm k tm -> do
            m_mb_tm
              >>= case _ of
                  Just _ -> m_mb_tm
                  Nothing -> k tm
        )
        (pure Nothing)
        system
    )
