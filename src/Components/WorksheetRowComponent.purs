module Components.WorksheetRowComponent
       ( Action
       , State
       , createState
       , _worksheetRow
       , _result
       , spec
       ) where

import Prelude
import Data.Lens (Lens', lens)
import React.DOM as R
import Thermite as T

import Data.Worksheet (WorksheetRow, WorksheetRowResult(..))

data Action = Changed

type State = { worksheetRow :: WorksheetRow
             , result       :: WorksheetRowResult }

createState :: WorksheetRow -> State
createState = { worksheetRow: _
              , result      : Nothing }

_worksheetRow :: Lens' State WorksheetRow
_worksheetRow = lens _.worksheetRow (_ { worksheetRow = _ })

_result :: Lens' State WorksheetRowResult
_result = lens _.result (_ { result = _ })

render :: forall props. T.Render State props Action
render dispatch _ state _ =
  [ R.p' [ R.text "Row: "
         , R.text $ show state.worksheetRow
         , R.br' []
         , R.text "Result: "
         , R.text $ show state.result
         ]
  ]

spec :: forall eff props. T.Spec eff State props Action
spec = T.simpleSpec T.defaultPerformAction render
