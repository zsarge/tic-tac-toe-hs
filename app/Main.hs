import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, listToMaybe)
import Data.List (intercalate)
import Data.List.Split (chunksOf)

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

type Board = V.Vector Cell

newBoard :: Board
newBoard = V.fromList [Num (UnsafeIndex x) | x <- [1..9]]

move :: Board -> Index -> Player -> Board
move board index player = V.modify (\v -> MV.write v i cell) board
  where cell = Player player -- wrap
        i = idx index - 1 -- unwrap + offset

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
    extractIndex (Num i) = i
    extractIndex _         = error "Unexpected value" -- This will never happen due to the filter

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

takeTurn :: Player -> Board -> IO Board
takeTurn player board = do
  index <- getMove player board
  putStrLn ("You selected " ++ (show index))
  let nextBoard = move board index player
  return nextBoard

checkWinner :: Board -> Maybe Player
checkWinner board = firstJust $ map match patterns
    where firstJust = listToMaybe . catMaybes

          match indicies = 
              if all (== starting) [(board V.! i) | i <- indicies] 
              then getPlayer starting
              else Nothing
                 where starting = board V.! (head indicies)
                       getPlayer (Player p) = Just p
                       getPlayer _ = Nothing

          patterns = [ [0, 1, 2] -- horizontal
                     , [3, 4, 5]
                     , [6, 7, 8]

                     , [0, 3, 6] -- vertical
                     , [1, 4, 7] 
                     , [2, 5, 8]

                     , [0, 4, 8] -- diagonal
                     , [2, 4, 6]
                     ]

playGame :: Player -> Board -> IO ()
playGame player board 
  | Just winner <- checkWinner board = putStrLn $ show winner ++ " wins!"
  | null (validMoves board) = putStrLn "Game Over!"
  | otherwise = do
      board' <- takeTurn player board
      putStrLn $ showBoard $ board'
      case player of
        X -> playGame O board'
        O -> playGame X board'

main :: IO ()
main = do
  let board = newBoard
  putStrLn $ showBoard board
  playGame X board

