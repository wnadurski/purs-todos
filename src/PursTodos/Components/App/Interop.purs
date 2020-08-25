module PursTodos.Components.App.Interop (app) where

import Data.Unit (Unit)
import Effect.Unsafe (unsafePerformEffect)
import PursTodos.Components.App (mkApp)
import React.Basic (JSX)
  

app :: Unit -> JSX
app = unsafePerformEffect mkApp