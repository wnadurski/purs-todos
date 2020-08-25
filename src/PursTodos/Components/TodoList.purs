module PursTodos.Components.TodoList (mkTodoListComponent) where

import Prelude
import Control.Bind (bindFlipped)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson, parseJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import PursTodos.Data.TodoList (Id, TodoItem(..), TodoList(..), addItem, getId, getText, mkTodoItem, mkTodoList, toggleItem)
import React.Basic.DOM (button, div, h1_, input, label_, span_, text, label)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, useEffect, useEffectOnce, useState)
import React.Basic.Hooks as Hooks
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

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

renderItem :: (Id -> Effect Unit) -> TodoItem -> JSX
renderItem toggle item =
  div
    { className: "todo-item"
    , children:
        [ label_
            [ span_ [ text $ getText item ]
            , input
                { type: "checkbox"
                , onChange: handler_ $ toggle (getId item)
                , checked:
                    case item of
                      TodoItem _ _ -> false
                      FinishedTodoItem _ _ -> true
                }
            ]
        ]
    }

itemsList :: TodoList -> (Id -> Effect Unit) -> Boolean -> Array JSX
itemsList (TodoList items) toggle showAll = unfinished <> finished
  where
  unfinished =
    items
      # ( filter \item -> case item of
            TodoItem _ _ -> true
            _ -> false
        )
      <#> renderItem toggle

  finished =
    items
      # ( filter \item -> case item of
            FinishedTodoItem _ _ -> showAll
            _ -> false
        )
      <#> renderItem toggle

storageKey :: String
storageKey = "items"

mkTodoListComponent :: Component Unit
mkTodoListComponent = do
  addRow <- mkAddRow
  ls <- localStorage =<< window
  let
    loadFromStorage :: Effect TodoList
    loadFromStorage = do
      value <- getItem storageKey ls
      log $ show value
      let
        mbList :: Maybe (TodoList)
        mbList =
          value
            <#> parseJson
            <#> bindFlipped decodeJson
            >>= \valueOrError -> case valueOrError of
                Left x -> Nothing
                Right x -> pure x
    
      log $ show mbList <> "first"
      pure $ fromMaybe (TodoList []) mbList

    saveInStorage :: TodoList -> Effect Unit
    saveInStorage list = do
      setItem storageKey (stringify (encodeJson list)) ls
  component "TodoList" \_ -> Hooks.do
    state /\ setState <- useState (mkTodoList [])
    showAll /\ setShowAll <- useState false
    useEffectOnce do
      log "Hello2"
      todoList <- loadFromStorage
      log $ show todoList <> " hehre"
      setState \_ -> todoList
      pure (pure unit)
    useEffect state do
      log "Hello"
      saveInStorage state
      pure (pure unit)
    let
      handleAdd "" = pure unit

      handleAdd text = do
        item <- mkTodoItem text
        setState $ flip addItem item

      handleToggle :: Id -> Effect Unit
      handleToggle id = setState \list -> toggleItem list id
    pure
      $ div
          { className: "todolist-container"
          , children:
              [ h1_ [ text "Super pure TodoList" ]
              , label
                  { className: "showall-container"
                  , children:
                      [ input { type: "checkbox", checked: showAll, onChange: handler_ $ setShowAll \prev -> not prev }
                      , span_ [ text "Pokaż wszystkie" ]
                      ]
                  }
              , addRow { onAdd: handleAdd }
              , div { className: "items-container", children: itemsList state handleToggle showAll }
              ]
          }
