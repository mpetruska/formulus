module Components.WorksheetRowComponent
       ( Action(..)
       , State
       , createState
       , _worksheetRow
       , _result
       , updateInputValue
       , spec
       ) where

import Prelude
import Data.Lens (Lens', lens, over)
import Data.Tuple (Tuple(..))
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

import Data.Identifier (getIdentifierRepresentation)
import Data.Worksheet (WorksheetRow(..), WorksheetRowResult(..))

data Action = Changed
            | InputChanged String

type State = { worksheetRow :: WorksheetRow
             , inputText    :: String
             , result       :: WorksheetRowResult }

createState :: WorksheetRow -> State
createState (Input input) = { worksheetRow: Input input
                            , inputText:    show input.value
                            , result:       Nothing
                            }
createState c             = { worksheetRow: c
                            , inputText:    ""
                            , result:       Nothing
                            }

_worksheetRow :: Lens' State WorksheetRow
_worksheetRow = lens _.worksheetRow (_ { worksheetRow = _ })

_result :: Lens' State WorksheetRowResult
_result = lens _.result (_ { result = _ })

updateInputValue :: Number -> State -> State
updateInputValue x = over _worksheetRow update
  where
    update (Input i) = Input $ i { value = x }
    update c         = c

render :: forall props. T.Render State props Action
render dispatch _ state _ =
  case Tuple state.worksheetRow state.result of
    Tuple (Input input) _ ->
        [ R.p'
          [ R.span [ RP.className "worksheet-label" ] [ R.text input.label
                                                      , R.text " :" ]
          , R.br' []
          , R.span [ RP.className "worksheet-identifier" ] [ R.text (getIdentifierRepresentation input.identifier)
                                                           , R.text " = " ]
          , R.input [ RP._type "number"
                    , RP.className "worksheet-input"
                    , RP.value state.inputText
                    , RP.onKeyUp  \e -> dispatch (InputChanged (unsafeCoerce e).target.value)
                    , RP.onChange \e -> dispatch (InputChanged (unsafeCoerce e).target.value) ] []
          ]
        ]
    Tuple (Calculation c) (CalculationResult result) ->
        [ R.p'
          [ R.span [ RP.className "worksheet-label" ] [ R.text c.label
                                                      , R.text " :" ]
          , R.br' []
          , R.span [ RP.className "worksheet-identifier" ] [ R.text (getIdentifierRepresentation c.identifier)
                                                           , R.text " = " ]
          , R.span [ RP.className "worksheet-result" ] [ R.text (show result) ]
          ]
        ]
    
    _ ->
      [ R.p' [ R.span [ RP.className "worksheet-programming-error" ] [ R.text "Ooops!" ] ]
      ]

performAction :: forall eff props. T.PerformAction eff State props Action
performAction (InputChanged s) _ _ = void $ T.modifyState (_ { inputText = s })
performAction _                _ _ = pure unit

spec :: forall eff props. T.Spec eff State props Action
spec = T.simpleSpec performAction render
