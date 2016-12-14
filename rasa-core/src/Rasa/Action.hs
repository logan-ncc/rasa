{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rasa.Action where

import Control.Monad.State
import Rasa.Types

execAction :: Store -> Action () -> IO Store
execAction store action = execStateT (runAction action) store

evalAction :: Store -> Action a -> IO a
evalAction store action = evalStateT (runAction action) store

execBufAction :: Buffer -> BufAction a -> IO Buffer
execBufAction buf = flip execStateT buf . getBufAction

runBufAction :: Buffer -> BufAction a -> IO (a, Buffer)
runBufAction buf = flip runStateT buf . getBufAction
