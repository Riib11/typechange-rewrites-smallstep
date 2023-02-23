module Main where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import RenderTerm (term)
import Syntax as Syntax

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI mainComponent unit body

type MainState
  = { term :: Syntax.Term
    }

mainComponent :: forall query input output m. H.Component query input output m
mainComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: _ -> MainState
  initialState _ =
    { term: Syntax.freshHoleTerm (Syntax.freshHoleType unit)
    }

  render state =
    HH.div
      [ HP.class_ (HH.ClassName "app") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "program") ]
          [ term state.term ]
      ]

  handleAction _input = pure unit

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
