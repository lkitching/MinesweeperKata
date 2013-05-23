module Transform where

import Data.Maybe
import Parser
import Board

data Adj = ABomb | Adj Int

transformSquare :: Mine -> [Mine] -> Adj
transformSquare Bomb _ = ABomb
transformSquare Clear ns = Adj $ length $ filter ((==) Bomb) ns

transform :: Board Mine -> Board Adj
transform board = mapi board mapSquare
  where mapSquare r c s = transformSquare s $ getNeighbours r c board