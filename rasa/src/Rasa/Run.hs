{-# language ExistentialQuantification, Rank2Types #-}
module Rasa.Run (rasa) where

import Rasa.Ext
import Rasa.State
import Rasa.Action

import Control.Lens
import Control.Concurrent.Async
import Control.Monad
import Data.Default (def)

handleEvent :: Store -> Action () -> IO Store
handleEvent = execAction

rasa :: [Action [Event]] -> Action () -> IO ()
rasa eventListeners extensions = eventLoop eventListeners extensions def

eventLoop ::  [Action [Event]] -> Action () -> Store -> IO ()
eventLoop eventListeners extensions store = do
  newStore <- handleEvent store extensions

  unless (newStore^.exiting) $ do
    asyncEventListeners <- traverse (async.evalAction newStore) eventListeners
    (_, nextEvents) <- waitAny asyncEventListeners
    let withEvents = (newStore & event .~ nextEvents)
    eventLoop eventListeners extensions withEvents
