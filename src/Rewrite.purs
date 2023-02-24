module Rewrite where

import Prelude
import Syntax
import Change (changePolyType, changeType)
import Control.Monad.Except (ExceptT)
import Data.Array (foldM, reverse, singleton, uncons, (:))
import Data.Array as Array
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Fixpoint (fixpointM)
import Utility (arrayuncurry1, arrayuncurry2)

-- TODO: maybe add effects later?
type RewriteM a
  = Effect a

runRewriteM :: forall a. RewriteM a -> Effect a
runRewriteM = identity

-- | Rule: the type of rewrite rules over terms
type Rule
  = Term -> RewriteM (Maybe Term)

applyRules :: Array Rule -> Term -> RewriteM (Maybe Term)
applyRules rules tm = case Array.uncons rules of
  Nothing -> pure Nothing
  Just { head: rule, tail: rules' } ->
    applyRule rule tm
      >>= case _ of
          Just tm' -> pure (Just tm')
          Nothing -> applyRules rules' tm

-- | applyRule: Just if the rule was applied somewhere; Nothing if the rule was
-- | not applied anywhere
applyRule :: Rule -> Term -> RewriteM (Maybe Term)
applyRule rule tm = do
  Console.log $ "applyRule: " <> show tm
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
          TopTerm top -> helper (arrayuncurry1 \bod -> TopTerm top { bod = bod }) [ top.bod ]
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

fixpointRules :: Array Rule -> Term -> RewriteM Term
fixpointRules rules =
  fixpointM
    ( foldM
        ( \m_mb_tm k tm -> do
            m_mb_tm
              >>= case _ of
                  Just _ -> m_mb_tm
                  Nothing -> k tm
        )
        (pure Nothing)
        rules
    )
