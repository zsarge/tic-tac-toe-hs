{-# LANGUAGE DataKinds #-}

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Text.Read (readMaybe)
import Control.Monad.ST (runST)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import GHC.TypeLits (Nat)
import Text.Printf

newtype Index = UnsafeIndex { idx :: Int } deriving (Eq, Ord)

instance Show Index where 
    show (UnsafeIndex index) = show index

makeIndex :: Int -> Maybe Index
makeIndex i
  | 1 <= i && i <= 9 = Just (UnsafeIndex i)
  | otherwise = Nothing
tryMakeIndex :: Maybe Int -> Maybe Index
tryMakeIndex (Just n) = makeIndex n
tryMakeIndex _ = Nothing

data Player = X | O deriving (Show, Eq)
data Cell = Player Player | Num Index deriving (Show, Eq)

toString :: Cell -> String
toString (Player X) = "X"
toString (Player O) = "O"
toString (Num x) = show x

-- Represents a tic-tac-toe board with a fixed size of 9
type Board = V.Vector Cell

-- "@9" refers to the type-level number 9
newBoard :: Board
newBoard = V.fromList [Num (UnsafeIndex x) | x <- [1..9]]

move :: Board -> Index -> Player -> Board
move board index player = V.modify (\v -> MV.write v i cell) board
  where cell = Player player -- wrap
        i = idx index -- unwrap

showBoard :: Board -> String
showBoard board = formatted
  where -- create 2d array of length-1 strings
        chunked = chunksOf 3 ((map toString . V.toList) board)
        -- seperate the numbers by pipes, padding with space and newline
        rows = map ((++ "\n") . (" " ++) . intercalate " | ") chunked 
        divider = "---+---+---\n"
        formatted = intercalate divider rows

validMoves :: Board -> V.Vector Index
validMoves vec = V.map extractIndex $ V.filter isNum vec
  where
    isNum (Num _) = True
    isNum _       = False
    extractIndex (Num idx) = idx
    extractIndex _         = error "Unexpected value" -- This will never happen due to the filter


-- getNumberInRange :: Int -> Int -> IO Int
-- getNumberInRange minVal maxVal = do
  -- putStrLn $ "Enter a number between " ++ show minVal ++ " and " ++ show maxVal ++ ":"
  -- input <- getLine
  -- case readMaybe input of
    -- Just n | n >= minVal && n <= maxVal -> return n
    -- _ -> do
      -- putStrLn "Invalid input. Please try again."
      -- getNumberInRange minVal maxVal


getMove :: Player -> Board -> IO Index
getMove player board = do
  putStrLn ("Player " ++ (show player) ++ "'s turn!")
  putStrLn "What index do you want to move to?"
  input <- getLine
  case (tryMakeIndex (readMaybe input)) of
    Just n | n `elem` (validMoves board) -> return n
    _ -> do
      putStrLn "Invalid input. Please try again."
      getMove player board

main :: IO ()
main = do
  -- let board = newBoard
  -- let index = UnsafeIndex 0
  -- print $ move board index X
  putStrLn $ showBoard newBoard
  getMove X newBoard
  return ()

