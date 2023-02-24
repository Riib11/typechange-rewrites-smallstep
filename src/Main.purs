module Main where

import Model
import Prelude
import Data.Array as Array
import Data.Lazy (force)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import RenderTerm (term)
import Rewrite (applyRules)
import Rules (rules)
import Syntax as Syntax
import Utility (unimplemented)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI mainComponent unit body

mainComponent :: forall query input output m. MonadEffect m => H.Component query input output m
mainComponent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just InitializeAction
              }
    }
  where
  initialState :: _ -> Model
  initialState _ =
    { term: Syntax.initialTerm
    , edits: []
    }

  render state =
    HH.div
      [ HP.class_ (HH.ClassName "app") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "program") ]
          [ HH.h2_ [ HH.text "Program" ]
          , term identity state.term
          ]
      , HH.div
          [ HP.class_ (HH.ClassName "edits") ]
          $ [ HH.h2_ [ HH.text "Edits" ] ]
          <> ( state.edits
                <#> ( \edit ->
                      HH.div
                        [ HP.class_ (HH.ClassName "edit")
                        , HE.onClick \_evt -> SetTermAction edit.term
                        ]
                        [ HH.text edit.label ]
                  )
            )
      ]

  handleAction = case _ of
    SetTermAction tm -> do
      H.liftEffect $ Console.log $ "SetTerm"
      H.modify_ \state -> state { term = force tm, edits = [] }
    SetEditsAction evt edits -> do
      H.liftEffect $ Console.log $ "SetEdits"
      H.liftEffect $ E.stopPropagation $ ME.toEvent evt
      H.modify_ \state -> state { edits = edits }
    StepAction -> do
      H.liftEffect $ Console.log $ "Step"
      state <- H.get
      (H.liftEffect $ applyRules rules state.term)
        >>= case _ of
            Nothing -> H.liftEffect $ Console.log "no update; at fixpoint"
            Just tm -> H.put state { term = tm, edits = [] }
    KeyDownAction _subId evt -> do
      let
        str = KE.key evt
      when (str `Array.elem` [ " " ])
        $ H.liftEffect
        $ E.preventDefault
        $ KE.toEvent evt
      if str == " " then do
        handleAction StepAction
      else
        pure unit
    InitializeAction -> do
      H.liftEffect $ Console.log "Initialize"
      document <- H.liftEffect $ document =<< window
      H.subscribe' \subId ->
        eventListener
          KET.keydown
          (HTMLDocument.toEventTarget document)
          -- (map ?a <<< KE.fromEvent)
          (KE.fromEvent >>> map (KeyDownAction subId))

-- void $ H.subscribe =<< emitter StepAction
-- emitter :: forall m. MonadAff m => Action -> m (HS.Emitter Action )
-- emitter act = do
--   {emitter}
{-
data Action
  = Increment
  | Decrement

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    Increment -> H.modify_ \state -> state + 1
    Decrement -> H.modify_ \state -> state - 1

-}
