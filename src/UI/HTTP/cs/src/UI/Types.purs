module UI.Types where

import Types (State())
import React
import Control.Monad.Eff.Ref
import Prelude

class AsHtml a where
    asHtml :: REF State -> a -> UI
