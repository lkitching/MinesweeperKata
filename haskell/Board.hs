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
               
initBoard :: Width -> Height -> ((Row, Col) -> a) -> Board a               
initBoard w@(Width wi) h@(Height hi) f = Board { width = w, height = h, squares = squares }
  where coords = [0..(hi-1)] >>= (\r -> map (\c -> (Row r, Col c)) [0..(wi-1)])
        squares = foldl (\m c -> M.insert c (f c) m) M.empty coords
        
get :: Row -> Col -> Board a -> Maybe a        
get r c = M.lookup (r, c) . squares

getRow :: Row -> Board a -> [a]
getRow r (Board { width = (Width w), squares = boardSquares }) =
  catMaybes $ map (\coord -> M.lookup coord boardSquares) $ map (\c -> (r, Col c)) [0..(w-1)]