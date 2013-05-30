module Squares where

data Mine = Clear | Bomb deriving (Eq, Show)
data Adj = ABomb | Adj Int

instance Show Adj where
  show ABomb = "*"
  show (Adj i) = show i