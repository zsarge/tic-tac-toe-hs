{-# LANGUAGE DataKinds #-}

import Data.Vector.Sized (Vector, generate) -- installed via "cabal install vector-sized"
import GHC.TypeLits (Nat)

data Piece = X | O

data Cell = Piece | Num Int deriving (Show, Eq)

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


move :: Board -> Index -> Index -> Piece -> Board
move board x y piece = newBoard

main :: IO ()
main = do 
    let board = newBoard
    let index = UnsafeMakeIndex 0
    print $ move board index index X 

