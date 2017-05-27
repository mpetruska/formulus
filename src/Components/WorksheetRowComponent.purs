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
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

import Data.Identifier (getIdentifierRepresentation)
import Data.Worksheet (CalculationResult(..), WorksheetRow(..), WorksheetRowResult(..))

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
render dispatch _ state _ = renderRow state.worksheetRow state.result
  where
    renderCalculationResult (Result r) = R.span [ RP.className "worksheet-result" ] [ R.text r                ]
    renderCalculationResult (Error  e) = R.span [ RP.className "worksheet-error"  ] [ R.text ("error: " <> e) ]
    
    renderRow (Input input) _ =
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
    renderRow (Calculation c) (CalculationResult result) =
        [ R.p'
          [ R.span [ RP.className "worksheet-label" ] [ R.text c.label
                                                      , R.text " :" ]
          , R.br' []
          , R.span [ RP.className "worksheet-identifier" ] [ R.text (getIdentifierRepresentation c.identifier)
                                                           , R.text " = " ]
          , renderCalculationResult result
          ]
        ]
    renderRow _ _ =
        [ R.p' [ R.span [ RP.className "worksheet-programming-error" ] [ R.text "Oops, there is a bug, please report!" ] ]
        ]

performAction :: forall eff props. T.PerformAction eff State props Action
performAction (InputChanged s) _ _ = void $ T.modifyState (_ { inputText = s })
performAction _                _ _ = pure unit

spec :: forall eff props. T.Spec eff State props Action
spec = T.simpleSpec performAction render
