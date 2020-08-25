module PursTodos.Data.TodoList where

import Prelude
import Effect (Effect)

type Id
  = Int

data TodoItem
  = TodoItem Id String
  | FinishedTodoItem Id String

getText :: TodoItem -> String
getText (TodoItem _ text) = text

getText (FinishedTodoItem _ text) = text

newtype TodoList
  = TodoList (Array TodoItem)

mkTodoList :: Array TodoItem -> TodoList
mkTodoList = TodoList

addItem :: TodoList -> TodoItem -> TodoList
addItem (TodoList list) item = TodoList (list <> [ item ])

toggleItem :: TodoList -> Id -> TodoList
toggleItem (TodoList list) idToToggle = TodoList (toggle <$> list)
  where
  toggle (TodoItem id text)
    | id == idToToggle = FinishedTodoItem id text

  toggle (FinishedTodoItem id text)
    | id == idToToggle = TodoItem id text

  toggle x = x

mkTodoItem :: String -> Effect TodoItem
mkTodoItem text = do
  id <- pure 5
  pure $ TodoItem id text
