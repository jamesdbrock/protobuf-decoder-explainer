module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State = { enabled :: Boolean }

data Action = Toggle

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  -- runUI Button.component unit body
  runUI component unit body

-- component :: forall q i o m. H.Component q i o m
-- component =
--   H.mkComponent
--     { unit
--     , render
--     , eval: H.mkEval $ H.defaultEval { }
--     }
--
-- render :: forall m. Unit -> H.ComponentHTML Unit () m
-- render _ =
--   HH.h1 [] [HH.text "Protobuf Decoder"]
--
component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    label = if state.enabled then "On" else "Off"
  in HH.div []
    [ HH.h1 [] [HH.text "Protobuf Decoder"]
    , HH.button
        [ HP.title label
        , HE.onClick \_ -> Just Toggle
        ]
        [ HH.text label ]
    ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle ->
    H.modify_ \st -> st { enabled = not st.enabled }