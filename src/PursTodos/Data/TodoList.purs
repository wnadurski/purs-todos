module PursTodos.Data.TodoList where

import Prelude
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID, emptyUUID, genUUID, parseUUID, toString)
import Effect (Effect)

newtype Id
  = Id UUID

derive instance eqId :: Eq Id

instance showId :: Show Id where
  show (Id uuid) = show uuid

instance decodeJsonId :: DecodeJson Id where
  decodeJson = decodeString >>> map parseUUID >>> map (fromMaybe emptyUUID) >>> map Id

instance encodeJsonId :: EncodeJson Id where
  encodeJson (Id uuid) = encodeString (toString uuid)

data TodoItem
  = TodoItem Id String
  | FinishedTodoItem Id String

derive instance eqTodoItem :: Eq TodoItem

instance showTodoItem :: Show TodoItem where
  show (TodoItem id str) = "TodoItem(" <> show id <> ", " <> str <> ")"
  show (FinishedTodoItem id str) = "FinishedTodoItem(" <> show id <> ", " <> str <> ")"

derive instance genericTodoItem :: Generic TodoItem _

instance decodeJsonTodoItem :: DecodeJson TodoItem where
  decodeJson = genericDecodeJson

instance encodeJsonTodoItem :: EncodeJson TodoItem where
  encodeJson = genericEncodeJson

getText :: TodoItem -> String
getText (TodoItem _ text) = text

getText (FinishedTodoItem _ text) = text

getId :: TodoItem -> Id
getId (TodoItem id _) = id

getId (FinishedTodoItem id _) = id

newtype TodoList
  = TodoList (Array TodoItem)

derive instance eqTodoList :: Eq TodoList

instance showTodoList :: Show TodoList where
  show (TodoList arr) = show arr

derive instance genericToDoList :: Generic TodoList _

instance decodeJsonToDoList :: DecodeJson TodoList where
  decodeJson = genericDecodeJson

instance encodeJsonTodoList :: EncodeJson TodoList where
  encodeJson = genericEncodeJson

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
  id <- genUUID
  pure $ TodoItem (Id id) text
