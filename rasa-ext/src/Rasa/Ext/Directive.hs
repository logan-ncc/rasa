module Rasa.Ext.Directive
  ( exit
  , insertTextAt
  , deleteCharAt
  , addBuffer
  , addBufferThen
  , bufDo
  , focusDo
  ) where

import Rasa.Ext
import Control.Monad.IO.Class

import Control.Lens
import qualified Data.Text as T
import Data.Monoid

bufDo :: BufAction () -> Action ()
bufDo = Action . zoom (buffers . traverse) . getBufAction

focusDo :: BufAction () -> Action ()
focusDo = Action . zoom focusedBuf . getBufAction

addBuffer :: T.Text -> Action ()
addBuffer txt = buffers %= (++[newBuffer txt])

addBufferThen :: T.Text -> BufAction a -> Action a
addBufferThen txt act = do
  (a, newBuf) <- liftIO $ runBufAction (newBuffer txt) act
  buffers %= (++[newBuf])
  return a

exit :: Action ()
exit = do
  exiting .= True
  event .= [Exit]

insertTextAt :: Int -> T.Text -> T.Text -> T.Text
insertTextAt i new txt = T.take i txt <> new <> T.drop i txt

deleteCharAt :: Int -> T.Text -> T.Text
deleteCharAt i txt = T.take i txt <> T.drop (i + 1) txt

