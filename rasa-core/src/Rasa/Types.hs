{-# LANGUAGE Rank2Types, TemplateHaskell, OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   GeneralizedNewtypeDeriving, FlexibleInstances,
   StandaloneDeriving #-}

module Rasa.Types (
  Store
                  , Action
                  , BufAction
                  , Buffer
                  , Editor
                  , runAction
                  , getBufAction
                  , Ext(..)
                  , Coord
                  , focused
                  , bufExts
                  , attrs
                  , text
                  , newBuffer
                  , exiting
                  , editor
                  , buffers
                  , extState
                  , fg
                  , bg
                  , style
                  , Color(..)
                  , Style(..)
                  , Attr(..)
                  , IAttr(..)
                  , event
                  , Event(..)
                  , Mod(..)
                  , iattr
  ) where

import Rasa.Types.Attribute
import Rasa.Types.Event
import Control.Lens
import Data.Text.Lens (packed)
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Typeable
import Data.Default

type Coord = (Int, Int)

data Ext = forall a. Show a => Ext a
deriving instance Show Ext

data Buffer = Buffer
  { _text :: T.Text
  , _bufExts :: M.Map TypeRep Ext
  -- This list must always remain sorted by offset
  , _attrs :: [IAttr]
  }

makeLenses ''Buffer
instance Show Buffer where
  show b = "<Buffer {text:" ++ show (b^..text.from packed.taking 30 traverse) ++ "...,\n\n"
           ++ "attrs: " ++ show (b^.attrs) ++ "\n\n"
           ++ "exts: " ++ show (b^.bufExts) ++ "}>\n\n"

newBuffer :: T.Text -> Buffer
newBuffer txt =
  Buffer
  { _text = txt
  , _bufExts = M.empty
  , _attrs = def
  }

data Editor = Editor {
    _buffers :: [Buffer]
  , _focused :: Int
  , _exiting :: Bool
} deriving Show

makeLenses ''Editor

instance Default Editor where
    def = Editor {
            _buffers=fmap newBuffer [ "Buffer 0\nHey! How's it going over there?\nI'm having just a splended time!\nAnother line for you sir?"
                                 , "Buffer 1\nHey! How's it going over there?\nI'm having just a splended time!\nAnother line for you sir?" ]
          , _focused=0
          , _exiting=False
             }


newtype Action a = Action
  { runAction :: StateT Store IO a
  } deriving (Functor, Applicative, Monad, MonadState Store, MonadIO)

newtype BufAction a = BufAction
  { getBufAction::StateT Buffer IO a
  } deriving (Functor, Applicative, Monad, MonadState Buffer, MonadIO)

data Store = Store
  { _event :: [Event]
  , _editor :: Editor
  , _extState :: M.Map TypeRep Ext
  } deriving (Show)

makeLenses ''Store

instance Default Store where
  def =
    Store
    { _event = [def]
    , _editor = def
    , _extState = M.empty
    }


