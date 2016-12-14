{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Rasa.Ext
  ( Action
  , Buffer
  , BufAction
  , Event(..)
  , Mod(..)
  , text
  , editor
  , exiting
  , ext
  , bufExt
  , allBufExt
  , event
  , iattr
  , attrs
  , fg
  , bg
  , style
  , Color(..)
  , Style(..)
  , IAttr(..)
  , Attr(..)
  ) where

import Rasa.Types
import Rasa.State
