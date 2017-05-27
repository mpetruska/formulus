module Components.WorksheetComponent
       ( State
       , _rows
       , _worksheet
       , _worksheetResults
       , Action
       , spec
       , initialState
       ) where

import Prelude
import Control.Coroutine (CoTransformer)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (fromFoldable) as A
import Data.Either (Either(..), either)
import Data.Foldable (fold, traverse_)
import Data.Lens (Lens', Prism', lens, over, prism, set, view)
import Data.List (List(..), deleteAt, fromFoldable, modifyAt, snoc, zipWith)
import Data.Maybe (Maybe, maybe)
import Data.String (Pattern(..), stripPrefix)
import Data.Tuple (Tuple(..), uncurry)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Location (hash, setHash) as DOM
import DOM.HTML.Window (location) as DOM
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T

import Components.WorksheetRowComponent as Row
import Data.Worksheet (Worksheet, WorksheetResults, decodeWorksheet, encodeWorksheet, runWorksheet)
import Operators (leftZipWith)
import Parsers (float, parseWith)

type State = { rows :: List Row.State }

_rows :: Lens' State (List Row.State)
_rows = lens _.rows (_ { rows = _ })

_worksheet :: Lens' State Worksheet
_worksheet = lens getter setter
  where
    getter state = A.fromFoldable $ map _.worksheetRow state.rows
    newRows worksheet = leftZipWith (set Row._worksheetRow) Row.createState (fromFoldable worksheet)
    setter state worksheet = state { rows = newRows worksheet state.rows }

_worksheetResults :: Lens' State WorksheetResults
_worksheetResults = lens getter setter
  where
    getter state = A.fromFoldable $ map _.result state.rows
    newRows results = zipWith (set Row._result) (fromFoldable results)
    setter state worksheet = state { rows = newRows worksheet state.rows }

calculateResults :: State -> State
calculateResults state = set _worksheetResults (runWorksheet $ view _worksheet state) state

defaultWorksheet :: Worksheet
defaultWorksheet = []

initialState :: forall e. Eff (dom :: DOM | e) State
initialState = do
  hash          <- DOM.window >>= DOM.location >>= DOM.hash
  let encoded   =  maybe hash id $ stripPrefix (Pattern "#!") hash
  let worksheet =  either (const defaultWorksheet) id $ decodeWorksheet encoded
  pure $ calculateResults $ set _worksheet worksheet { rows: Nil }

encodeWorksheetToHash :: forall e. State -> Eff (dom :: DOM | e) Unit
encodeWorksheetToHash state = do
  location      <- DOM.window >>= DOM.location
  let encoded   =  "#!" <> (encodeWorksheet $ view _worksheet state)
  DOM.setHash encoded location

data Action = RowAction Int Row.Action
            | AddRow

_RowAction :: Prism' Action (Tuple Int Row.Action)
_RowAction = prism (uncurry RowAction) \ta ->
  case ta of
    RowAction i a -> Right (Tuple i a)
    _             -> Left  ta

updateState :: forall eff. (State -> State) -> CoTransformer (Maybe State) (State -> State) (Aff (dom :: DOM | eff)) Unit
updateState update = void do
  maybeNewState <- T.modifyState (update >>> calculateResults)
  liftEff $ traverse_ encodeWorksheetToHash maybeNewState

updateRowState :: forall eff. Int -> (Row.State -> Row.State) -> CoTransformer (Maybe State) (State -> State) (Aff (dom :: DOM | eff)) Unit
updateRowState i f = updateState update
  where
    update state = maybe state (state { rows = _ }) $ modifyAt i f state.rows

performAction :: forall eff props. T.PerformAction (dom :: DOM | eff) State props Action
performAction (RowAction i (Row.InputChanged s)) _ _ = updateRowState i update
  where
    eitherFloatValue = parseWith float s
    update = either (const id) Row.updateInputValue eitherFloatValue

performAction (RowAction i (Row.EditingDone)) _ _ = updateRowState i (Row.save)
performAction AddRow                          _ _ = updateState (over _rows $ (flip snoc) Row.newDefaultRow)
performAction (RowAction i (Row.Delete))      _ _ = updateState (over _rows $ delete)
  where
    delete rows = maybe rows id $ deleteAt i rows
    
performAction _ _ _ = pure unit

renderAddRow :: forall props. T.Render State props Action
renderAddRow dispatch _ state _ =
  [ R.a [ RP.className "add"
        , RP.onClick \_ -> dispatch AddRow ] [ R.text "add" ]
  ]

spec :: forall eff props. T.Spec (dom :: DOM | eff) State props Action
spec = fold
        [ T.focus _rows _RowAction $ T.foreach \_ -> Row.spec
        , T.simpleSpec performAction renderAddRow
        ]
