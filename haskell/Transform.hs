module Transform where

import Data.Maybe
import Data.List
import Parser
import Board
import Squares

transformSquare :: Mine -> [Mine] -> Adj
transformSquare Bomb _ = ABomb
transformSquare Clear ns = Adj $ length $ filter ((==) Bomb) ns

transform :: Board Mine -> Board Adj
transform board = mapi board mapSquare
  where mapSquare r c s = transformSquare s $ getNeighbours r c board
        
formatBoard :: Int -> Board Adj -> String
formatBoard num board = unlines ["Field #" ++ show num ++ ":", show board]
        
boardStrings :: [Board Adj] -> [String]
boardStrings = reverse . snd . foldl (\(n, acc) b -> (n+1, (formatBoard n b):acc)) (1, [])
        
formatBoards :: [Board Adj] -> String
formatBoards = intercalate "\n" . boardStrings
   
