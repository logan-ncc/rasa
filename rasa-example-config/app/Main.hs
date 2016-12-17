module Main where

import Rasa.Run (rasa)
import Rasa.Ext.Vim
import Rasa.Ext.Files
import Rasa.Ext.Style
import Rasa.Ext.StatusBar
import Rasa.Ext.Logger
import Rasa.Ext.Cursors
import Rasa.Renderer.Slate

main :: IO ()
main = rasa [slateEvent] $ do
  style
  vim
  statusBar
  files
  cursorMain
  logger
  slate
