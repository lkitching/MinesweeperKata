module Parser where

import Data.List
import Data.Char
import Board
import Squares


parseMine :: Char -> Mine
parseMine '*' = Bomb
parseMine '.' = Clear
parseMine _   = error "No parse - expected '*' or '.'"

parseDimBoard :: Width -> Height -> String -> (Board Mine, String)
parseDimBoard width@(Width w) height@(Height h) boardString =
  let (boardData, rest) = splitAt (w * h) boardString
      board = initRowWise width height $ map parseMine boardData
  in  (board, rest)

parseBoard :: String -> Maybe (Board Mine, String)
parseBoard (h:' ':w:hs) =
  let width = read [w] :: Int
      height = read [h] :: Int
  in  if (width, height) == (0, 0) then Nothing else Just $ parseDimBoard (Width width) (Height height) hs
parseBoard _ = error "Invalid board - expected [height width data]"

parseBoards :: String -> [Board Mine]
parseBoards = unfoldr parseBoard . filter (not . isNewlineChar)
  where isNewlineChar c = c `elem` ['\n', '\r']

