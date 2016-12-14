{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors
  ( moveCursorBy
  , moveCursorTo
  , moveCursorCoord
  , cursors
  , deleteChar
  , insertText
  , findNext
  , findPrev
  ) where

import Rasa.Ext
import Rasa.Ext.Directive

import Control.Monad
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader

import Control.Lens.Text as TL
import Data.Typeable
import Data.Monoid

import qualified Data.Text as T
type Coord = (Int, Int)
newtype Cursor = Cursor
  { _curs :: Int
  } deriving (Show, Typeable)

makeLenses ''Cursor

cursor :: Traversal' Buffer Int
cursor = bufExt._Just.curs

fUse :: MonadState s f => Getting (Data.Monoid.Endo b) s b -> f b
fUse l = get <&> (^?!l)

displayCursor ::  BufAction ()
displayCursor = do
  c <- fUse cursor
  attrs .= [iattr c (style ReverseVideo), iattr (c+1) (style DefStyle)]

cursors :: Action ()
cursors = do
  evt <- use event
  -- Initialize all buffers
  when (Init `elem` evt) $ allBufExt .= (Just $ Cursor 0)
  bufDo displayCursor

moveCursorTo :: Int -> BufAction ()
moveCursorTo n = do
  mx <- use (text.to T.length)
  cursor .= clamp 0 mx n

moveCursorBy :: Int -> BufAction ()
moveCursorBy n = do
  mx <- use $ text.to T.length
  cursor %= clamp 0 mx . (+n)

moveCursorCoord :: Coord -> BufAction ()
moveCursorCoord coord = do
  txt <- use text
  cursor.asCoord txt %= addPair coord
  where
    addPair (a, b) (a', b') = (a + a', b + b')

deleteChar :: BufAction ()
deleteChar = do
  c <- fUse cursor
  text %= deleteCharAt c

insertText :: T.Text -> BufAction ()
insertText txt = do
  c <- fUse cursor
  text %= insertTextAt c txt

findNext :: T.Text -> BufAction ()
findNext txt = do
  c <- fUse cursor
  n <- use $ text.after c.tillNext txt.to T.length
  moveCursorBy n

findPrev :: T.Text -> BufAction ()
findPrev txt = do
  c <- fUse cursor
  n <- use $ text.before c.tillPrev txt.to T.length
  moveCursorBy (-n)

asCoord :: T.Text -> Iso' Int Coord
asCoord txt = iso (toCoord txt) (toOffset txt)

toOffset :: T.Text -> Coord -> Int
toOffset t (row, col) = clamp 0 (T.length t) $ rowCount + clamp 0 rowLen col
  where
    rowCount = t ^. intillNextN row "\n" . to T.length
    rowLen = T.length $ T.lines t ^. ix row

toCoord :: T.Text -> Int -> Coord
toCoord txt offset = flip runReader txt $ do
  row <- view $ before offset . TL.matching "\n" . to T.length
  col <-
    case row of
      0 -> return offset
      _ -> view $ before offset . tillPrev "\n" . to T.length
  return (row, col)

clamp :: Int -> Int -> Int -> Int
clamp mn mx n
  | n < mn = mn
  | n > mx = mx
  | otherwise = n
