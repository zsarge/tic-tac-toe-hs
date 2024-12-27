{-# LANGUAGE DataKinds #-}

import qualified Data.Vector.Sized as V
import Data.List (intercalate)
import Data.Vector.Sized (Vector, generate, toList)
import Data.List.Split (chunksOf)
import GHC.TypeLits (Nat)
import Text.Printf

newtype Index = UnsafeIndex { i :: Int } deriving (Eq)

instance Show Index where 
    show (UnsafeIndex index) = show index

makeIndex :: Int -> Maybe Index
makeIndex i
  | 1 <= i && i <= 9 = Just (UnsafeIndex i)
  | otherwise = Nothing

data Player = X | O deriving (Show, Eq)
data Cell = Player Player | Num Index deriving (Show, Eq)

toString :: Cell -> String
toString (Player X) = "X"
toString (Player O) = "O"
toString (Num x) = show x

-- Represents a tic-tac-toe board with a fixed size of 9
type Board = Vector 9 Cell

-- "@9" refers to the type-level number 9
newBoard :: Board
newBoard = generate @9 (\x -> Num (UnsafeIndex $ (fromIntegral x) + 1))

move :: Board -> Index -> Player -> Board
move board index player = newBoard

showBoard :: Board -> String
showBoard board = formatted
  where -- create 2d array of length-1 strings
        chunked = chunksOf 3 ((map toString . toList) board)
        -- seperate the numbers by pipes, padding with space and newline
        rows = map ((++ "\n") . (" " ++) . intercalate " | ") chunked 
        divider = "---+---+---\n"
        formatted = intercalate divider rows

main :: IO ()
main = do
  -- let board = newBoard
  -- let index = UnsafeIndex 0
  -- print $ move board index X
  putStrLn $ showBoard newBoard

