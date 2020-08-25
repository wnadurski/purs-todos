module PursTodos.Components.TodoList (mkTodoListComponent) where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import PursTodos.Data.TodoList (TodoItem(..), TodoList(..), addItem, getText, mkTodoItem, mkTodoList)
import React.Basic.DOM (button, div, h1_, input, label_, span_, text)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, useState)
import React.Basic.Hooks as Hooks

mkAddRow :: Component ({ onAdd :: String -> Effect Unit })
mkAddRow =
  component "AddRow" \{ onAdd } -> Hooks.do
    state /\ setState <- useState ""
    pure
      $ div
          { className: "todolist__addRow"
          , children:
              [ input
                  { className: "todolist__addRow__input"
                  , value: state
                  , onChange: handler targetValue \value -> setState \_ -> fromMaybe "" value
                  }
              , button
                  { className: "todolist__addRow__button"
                  , children: [ text "Dodaj" ]
                  , onClick: handler_ $ onAdd state
                  }
              ]
          }

itemsList :: TodoList -> Array JSX
itemsList (TodoList items) =
  items
    <#> \item ->
        div
          { className: "todo-item"
          , children:
              [ label_
                  [ span_ [ text $ getText item ]
                  , input
                      { type: "checkbox"
                      , checked:
                          case item of
                            TodoItem _ _ -> false
                            FinishedTodoItem _ _ -> true
                      }
                  ]
              ]
          }

mkTodoListComponent :: Component Unit
mkTodoListComponent = do
  addRow <- mkAddRow
  component "TodoList" \_ -> Hooks.do
    state /\ setState <- useState (mkTodoList [])
    let
      handleAdd "" = pure unit

      handleAdd text = do
        item <- mkTodoItem text
        setState $ flip addItem item
    pure
      $ div
          { className: "todolist-container"
          , children:
              [ h1_ [ text "Super pure TodoList" ]
              , addRow { onAdd: handleAdd }
              , div { className: "items-container", children: itemsList state }
              ]
          }
