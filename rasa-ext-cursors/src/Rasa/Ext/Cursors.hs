{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors
  ( moveCursorsBy
  , moveCursorsBy'
  , moveCursorsTo
  -- , moveCursorCoord
  , cursorMain
  , deleteChar
  , insertText
  , findNext
  , findPrev
  ) where

import Rasa.Ext
import Rasa.Ext.Scheduler
import Rasa.Ext.Directive
import Rasa.Ext.Style

import Control.Lens
import Control.Monad.Reader

import Control.Lens.Text as TL
import Data.Typeable
import Data.Default

import qualified Data.Text as T
type Coord = (Int, Int)
newtype Cursors = Cursors
  { _cursors' :: [Int]
  } deriving (Show, Typeable)

makeLenses ''Cursors

instance Default Cursors where
  def = Cursors {
  _cursors'=[0, 5]
}

-- cursor :: Traversal' Buffer Int
-- cursor = cursors.traverse

cursors :: Traversal' Buffer [Int]
cursors = bufExt.cursors'.reversed

cursorDo :: (Int -> BufAction a) -> BufAction [a]
cursorDo f = do
  c <- use cursors
  mapM f c

cursorDo_ :: (Int -> BufAction a) -> BufAction ()
cursorDo_ = void . cursorDo

moveCursorsTo :: Int -> BufAction ()
moveCursorsTo loc = cursors .= [loc]

moveCursorsBy :: (Int -> BufAction Int) -> BufAction ()
moveCursorsBy f = do
  oldCursors <- use cursors
  newCursors <- mapM f oldCursors
  cursors.partsOf each .= zipWith (+) oldCursors newCursors

moveCursorsBy' :: Int -> BufAction ()
moveCursorsBy' i = cursors.traverse %= (+i)

displayCursor ::  BufAction ()
displayCursor = cursorDo_ setStyle
    where 
      setStyle :: Int -> BufAction ()
      setStyle c = addStyle (IStyle c (flair ReverseVideo)) >> addStyle (IStyle (c+1) (flair DefFlair))

cursorMain :: Scheduler ()
cursorMain = beforeRender $ bufDo displayCursor

-- moveCursorTo :: Int -> BufAction Int
-- moveCursorTo n = do
--   mx <- use (text.to T.length)
--   return $ clamp 0 mx n

-- moveCursorBy :: Int -> Int -> BufAction Int
-- moveCursorBy n c = do
--   mx <- use $ text.to T.length
--   return $ clamp 0 mx (c + n)

-- moveCursorCoord :: Coord -> BufAction ()
-- moveCursorCoord coord = do
--   txt <- use text
--   cursor.asCoord txt %= addPair coord
--   where
--     addPair (a, b) (a', b') = (a + a', b + b')

deleteChar :: BufAction ()
deleteChar = cursorDo_ deleteCharAt

insertText :: T.Text -> BufAction ()
insertText txt = cursorDo_ $ flip insertTextAt txt

findNext :: T.Text -> BufAction ()
findNext txt = moveCursorsBy distNext
  where
    distNext :: Int -> BufAction Int
    distNext c = use (text.after c.tillNext txt.to T.length)

findPrev :: T.Text -> BufAction ()
findPrev txt = moveCursorsBy distPrev
  where
    distPrev :: Int -> BufAction Int
    distPrev c = use (text.before c.tillPrev txt.to T.length.to negate)

clamp :: Int -> Int -> Int -> Int
clamp mn mx n
  | n < mn = mn
  | n > mx = mx
  | otherwise = n

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

