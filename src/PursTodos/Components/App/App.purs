module PursTodos.Components.App where

import Prelude
import PursTodos.Components.TodoList (mkTodoListComponent)
import React.Basic.DOM (text)
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as Hooks

mkApp :: Component Unit
mkApp = do
  todoList <- mkTodoListComponent
  component "App" \props -> Hooks.do
    pure $ todoList unit
