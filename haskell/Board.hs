module Board where

import qualified Data.Map as M
import Data.Maybe

newtype Row = Row Int deriving (Show, Ord, Eq)
newtype Col = Col Int deriving (Show, Ord, Eq)
newtype Height = Height Int deriving (Show, Ord, Eq)
newtype Width = Width Int deriving (Show, Ord, Eq)

data Board a = Board { width :: Width,
                       height :: Height,
                       squares :: M.Map (Row, Col) a
                     }
               
instance Show a => Show (Board a) where
  show (Board { width = (Width w), height = (Height h), squares = squares }) = unlines $ map showRow [0..(h-1)]
    where showRow r = concat $ map (show . fromJust . (flip M.lookup $ squares)) [(Row r, Col c) | c <- [0..(h-1)]]
          
initRowWise :: Width -> Height -> [a] -> Board a
initRowWise width@(Width w) height@(Height h) boardData =
  let boardSquares = foldl (\m (c, v) -> M.insert c v m) M.empty $ zip coords boardData
  in  Board { width = width, height = height, squares = boardSquares }
  where coords = [(Row r, Col c) | r <- [0..(h-1)], c <- [0..(w-1)]]
               
get :: Row -> Col -> Board a -> Maybe a        
get r c = M.lookup (r, c) . squares

getRow :: Row -> Board a -> [a]
getRow r (Board { width = (Width w), squares = boardSquares }) =
  catMaybes $ map (\coord -> M.lookup coord boardSquares) $ map (\c -> (r, Col c)) [0..(w-1)]
  
getNeighbours :: Row -> Col -> Board a -> [a]
getNeighbours (Row r) (Col c) board@(Board { squares = squares }) = catMaybes $ map (\(row, col) -> get row col board) nSquares
  where nSquares = [(Row (r + ro), Col (c + co)) | ro <- [-1,0,1], co <- [-1,0,1], (ro, co) /= (0, 0)]
        
mapi :: Board a -> (Row -> Col -> a -> b) -> Board b
mapi (Board { width = w, height = h, squares = squares }) f =
  Board { width = w, height = h, squares = mappedSquares }
 where mappedSquares = M.mapWithKey (uncurry f) squares
  
