module UI.Types where

import Types (State())
import React.Types (Component())
import Control.Monad.Eff.Ref

class AsHtml a where
    asHtml :: RefVal State -> a -> Component

