module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foldable (traverse_)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document) as DOM
import DOM.Node.Types (ElementId(..))
import DOM.Node.NonElementParentNode (getElementById) as DOM
import React (createFactory)
import ReactDOM (render)
import Thermite as T

import Components.WorksheetComponent (initialState, spec) as W

main :: forall e. Eff (console :: CONSOLE, dom :: DOM.DOM | e) Unit
main = void do
  let props     =  unit
  state         <- W.initialState
  let component =  T.createClass W.spec state
  doc           <- DOM.window >>= DOM.document
  let document  =  htmlDocumentToNonElementParentNode doc
  container     <- DOM.getElementById (ElementId "app") document
  traverse_ (render (createFactory component props)) container
