module Components.WorksheetRowComponent
       ( Action(..)
       , State
       , _worksheetRow
       , _isLocked
       , _result
       , createState
       , newDefaultRow
       , updateInputValue
       , save
       , spec
       ) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Lens (Lens', lens, over)
import Data.String (joinWith, null)
import Data.Validation.Semigroup (unV)
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Text.Parsing.Parser (parseErrorMessage)
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

import Data.Formula (prettyPrintFormula, parseFormula)
import Data.Identifier (defaultIdentifier, identifier, getIdentifierRepresentation)
import Data.Worksheet (CalculationResult(..), WorksheetRow(..), WorksheetRowResult(..), calculation, input)
import Parsers (int, parseWith)

data Action = Changed
            | InputChanged          String
            | SwitchToEdit
            | EditingDone
            | EditingCannotSave
            | EditingCancelled
            | Delete
            | LabelTextChanged      String
            | IdentifierTextChanged String
            | FormulaTextChanged    String
            | PrecisionTextChanged  String

type State = { worksheetRow    :: WorksheetRow
             , isLocked        :: Boolean
             , inputText       :: String
             , result          :: WorksheetRowResult
             , isEditing       :: Boolean
             , canSave         :: Boolean
             , labelText       :: String
             , identifierText  :: String
             , identifierError :: Boolean
             , formulaText     :: String
             , formulaError    :: Boolean
             , precisionText   :: String
             , precisionError  :: Boolean
             }

_worksheetRow :: Lens' State WorksheetRow
_worksheetRow = lens _.worksheetRow (_ { worksheetRow = _ })

_isLocked :: Lens' State Boolean
_isLocked = lens _.isLocked (_ { isLocked = _ })

_result :: Lens' State WorksheetRowResult
_result = lens _.result (_ { result = _ })

newState :: WorksheetRow -> String -> State
newState worksheetRow inputText = { worksheetRow:    worksheetRow
                                  , isLocked:        false
                                  , inputText:       inputText
                                  , result:          Nothing
                                  , isEditing:       false
                                  , canSave:         false
                                  , labelText:       ""
                                  , identifierText:  ""
                                  , identifierError: false
                                  , formulaText:     ""
                                  , formulaError:    false
                                  , precisionText:   ""
                                  , precisionError:  false
                                  }

createState :: WorksheetRow -> State
createState (Input input) = newState (Input input) (show input.value)
createState c             = newState c             ""

newDefaultRow :: State
newDefaultRow = switchToEdit $ createState $ input "" defaultIdentifier 0.0

updateInputValue :: Number -> State -> State
updateInputValue x = over _worksheetRow update
  where
    update (Input i) = Input $ i { value = x }
    update c         = c

switchToEdit :: State -> State
switchToEdit state = switch state.worksheetRow
  where
    switch (Input i)       = state { isEditing       = true
                                   , canSave         = true
                                   , labelText       = i.label
                                   , identifierText  = getIdentifierRepresentation i.identifier
                                   , identifierError = false
                                   , formulaText     = ""
                                   , formulaError    = false
                                   , precisionText   = "0"
                                   , precisionError  = false
                                   }
    switch (Calculation c) = state { isEditing       = true
                                   , canSave         = true
                                   , labelText       = c.label
                                   , identifierText  = getIdentifierRepresentation c.identifier
                                   , identifierError = false
                                   , formulaText     = prettyPrintFormula c.formula
                                   , formulaError    = false
                                   , precisionText   = show c.precision
                                   , precisionError  = false
                                   }

save :: State -> State
save state = saveStateToRow state.labelText state.identifierText state.formulaText state.precisionText
  where
    maybeIdentifier   = identifier >>> unV (joinWith ", " >>> Left) Right
    parseFormula' f   = lmap parseErrorMessage $ parseFormula f
    parseInt i        = lmap parseErrorMessage $ parseWith int i
    createInput       l i     = input l       <$> maybeIdentifier i <*> pure 0.0
    createCalculation l i f p = calculation l <$> maybeIdentifier i <*> parseFormula' f <*> parseInt p
    done = state { worksheetRow = _
                 , inputText    = "0"
                 , isEditing    = false
                 }
    saveStateToRow l i "" _ = either (\_ -> validate state) done $ createInput l i
    saveStateToRow l i f  p = either (\_ -> validate state) done $ createCalculation l i f p

render :: forall props. T.Render State props Action
render dispatch _ state _ = renderRow state.isEditing state.worksheetRow state.result
  where
    renderCalculationResult (Result r) = R.span [ RP.className "worksheet-result" ] [ R.text r                ]
    renderCalculationResult (Error  e) = R.span [ RP.className "worksheet-error"  ] [ R.text ("error: " <> e) ]
    
    dispatchValueChanged :: forall eff e. (String -> Action) -> e -> R.EventHandlerContext eff props State Unit
    dispatchValueChanged action e = dispatch $ action (unsafeCoerce e).target.value
    
    editableInput _type className value action error =
      R.input [ RP._type _type
              , RP.className  if error state then className <> " error" else className
              , RP.value    $ value state
              , RP.onKeyUp  $ dispatchValueChanged action
              , RP.onChange $ dispatchValueChanged action
              ] []
    
    editableWorksheetRowButtons =
      R.div [ RP.className "worksheet-row-buttons" ]
            [ R.a [ RP.className "edit"
                  , RP.onClick \_ -> dispatch SwitchToEdit ] [ R.text "edit" ]
            , R.a [ RP.className "delete"
                  , RP.onClick \_ -> dispatch Delete ] [ R.text "delete" ]
            ]
    
    withDefaultWorksheetRowButtons next =
      if not state.isLocked
        then [editableWorksheetRowButtons] <> next
        else next
    
    editingWorksheetRowButtons =
      R.div [ RP.className "worksheet-row-buttons" ]
            [ R.a [ RP.className "done"
                  , RP.onClick \_ -> dispatch if state.canSave then EditingDone else EditingCannotSave ] [ R.text "done" ]
            , R.a [ RP.className "cancel"
                  , RP.onClick \_ -> dispatch EditingCancelled ] [ R.text "cancel" ]
            ]
    
    renderRow true _ _ =
        [ R.div [ RP.className "worksheet-edit-row" ]
          [ editingWorksheetRowButtons
          , R.label' [ R.text "label: " ]
          , editableInput "text" "worksheet-label-edit"      _.labelText      LabelTextChanged      (const false)
          , R.br' []
          , R.label' [ R.text "identifier: " ]
          , editableInput "text" "worksheet-identifier-edit" _.identifierText IdentifierTextChanged _.identifierError
          , R.br' []
          , R.label' [ R.text "formula: " ]
          , editableInput "text" "worksheet-formula-edit"    _.formulaText    FormulaTextChanged    _.formulaError
          , R.br' []
          , R.label' [ R.text "precision: " ]
          , editableInput "text" "worksheet-precision-edit"  _.precisionText  PrecisionTextChanged  _.precisionError
          ]
        ]
    renderRow _ (Input input) _ =
        [ R.div [ RP.className "worksheet-row" ]
          ( withDefaultWorksheetRowButtons
              [ R.span [ RP.className "worksheet-label" ] [ R.text input.label
                                                          , R.text " :" ]
              , R.br'  []
              , R.span [ RP.className "worksheet-identifier" ] [ R.text (getIdentifierRepresentation input.identifier)
                                                               , R.text " = " ]
              , editableInput "number" "worksheet-input" _.inputText InputChanged (const false)
              ]
          )
        ]
    renderRow _ (Calculation c) (CalculationResult result) =
        [ R.div [ RP.className "worksheet-row" ]
          ( withDefaultWorksheetRowButtons
              [ R.span [ RP.className "worksheet-label" ] [ R.text c.label
                                                          , R.text " :" ]
              , R.br'  []
              , R.span [ RP.className "worksheet-identifier" ] [ R.text (getIdentifierRepresentation c.identifier)
                                                               , R.text " = " ]
              , renderCalculationResult result
              ]
          )
        ]
    renderRow _ _ _ =
        [ R.div [ RP.className "worksheet-row" ]
          [ R.span [ RP.className "worksheet-programming-error" ]
                   [ R.text "Oops, there is a bug, please report!" ]
          ]
        ]

validate :: State -> State
validate state = state { canSave         = not (identifierError || formulaError || precisionError)
                       , identifierError = identifierError
                       , formulaError    = formulaError
                       , precisionError  = precisionError
                       }
  where
   identifierError = unV (const true) (const false) $ identifier state.identifierText
   formulaError    = if null state.formulaText
                       then false
                       else either (const true) (const false) $ parseFormula state.formulaText
   precisionError  = either (const true) (const false) $ parseWith int state.precisionText

performAction :: forall eff props. T.PerformAction eff State props Action
performAction (InputChanged s)          _ _ = void $ T.modifyState (_ { inputText = s })
performAction SwitchToEdit              _ _ = void $ T.modifyState (switchToEdit)
performAction EditingCancelled          _ _ = void $ T.modifyState (_ { isEditing = false })
performAction EditingCannotSave         _ _ = void $ T.modifyState validate
performAction (LabelTextChanged s)      _ _ = void $ T.modifyState (_ { labelText      = s })
performAction (IdentifierTextChanged s) _ _ = void $ T.modifyState (_ { identifierText = s } >>> validate)
performAction (FormulaTextChanged s)    _ _ = void $ T.modifyState (_ { formulaText    = s } >>> validate)
performAction (PrecisionTextChanged s)  _ _ = void $ T.modifyState (_ { precisionText  = s } >>> validate)
performAction _                         _ _ = pure unit

spec :: forall eff props. T.Spec eff State props Action
spec = T.simpleSpec performAction render
