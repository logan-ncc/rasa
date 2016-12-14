module Rasa.Ext.Attributes where

import Rasa.Ext
import Control.Lens
import Data.List (insert)

-- Inserts an attribute into a buffer's attr list in sorted order
addAttr :: IAttr -> Buffer -> Buffer
addAttr attr = attrs %~ insert attr
