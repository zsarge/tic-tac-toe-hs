{-# LANGUAGE DataKinds #-}

import qualified Data.Vector.Sized as V
import Data.List (intercalate)
import Data.Vector.Sized (Vector, generate, toList)
import Data.List.Split (chunksOf)
import GHC.TypeLits (Nat)
import Text.Printf

data Player = X | O deriving (Show, Eq)
data Cell = Player Player | Num Int deriving (Show, Eq)

toString :: Cell -> String
toString (Player X) = "X"
toString (Player O) = "O"
toString (Num x) = show x

-- Represents a tic-tac-toe board with a fixed size of 9
type Board = Vector 9 Cell

-- "@9" refers to the type-level number 9
newBoard :: Board
newBoard = generate @9 (\x -> Num (fromIntegral x))

newtype Index = UnsafeMakeIndex { i :: Int }
makeIndex :: Int -> Maybe Index
makeIndex i
  | 1 <= i && i <= 9 = Just (UnsafeMakeIndex i)
  | otherwise = Nothing

move :: Board -> Index -> Player -> Board
move board index player = newBoard

showBoard :: Board -> String
showBoard board = formatted
  where allData = (map toString . toList) board
        chunked = chunksOf 3 allData
        rows = map ((++ "\n") . (" " ++) . intercalate " | ") chunked 
        formatted = intercalate divider rows
        divider = "---+---+---\n"

main :: IO ()
main = do
  -- let board = newBoard
  -- let index = UnsafeMakeIndex 0
  -- print $ move board index X
  putStrLn $ showBoard newBoard

