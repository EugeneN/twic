module UI.Types where

import React.Types (Component())

class AsHtml a where
    asHtml :: a -> Component
