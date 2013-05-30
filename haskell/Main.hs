import Control.Monad
import System.IO
import System.Environment

import Parser
import Transform

parseTransformWrite :: String -> IO ()
parseTransformWrite = putStr . formatBoards . (map transform) . parseBoards

minesweeper :: FilePath -> IO ()
minesweeper path = withFile path ReadMode ((=<<) parseTransformWrite . hGetContents)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> minesweeper path
    _ -> putStrLn "Usage minesweeper inputFile"