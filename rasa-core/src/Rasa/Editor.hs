{-# LANGUAGE TemplateHaskell, Rank2Types, OverloadedStrings #-}
module Rasa.Editor (
    Editor
  , focusedBuf
  , focused
  , buffers
  , buf
  , exiting
) where

import Control.Lens
import Data.Default (def, Default(..))
import Rasa.Types

import Rasa.Buffer


focusedBuf :: Lens' Editor Buffer
focusedBuf = lens getter (flip setter)
    where getter = do
            foc <- view focused
            -- TODO use ix here and make it safe??
            view (buffers.to (!! foc))

          setter a = do
            foc <- view focused
            set (buffers . ix foc) a

buf :: Int -> Traversal' Editor Buffer
buf bufN = buffers.ix bufN
