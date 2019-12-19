module Lib where

import           Data.Char (isDigit, ord)
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

data Board = Board {
    filled :: Int
  , points :: Map Point Int
} deriving Eq

instance Show Board where
  show b@(Board f _) =
    showBoard b ++ "\n " ++ show (81 - f) ++ " spots empty"

empty :: Board
empty = Board 0 M.empty

insert :: (Point, Int) -> Board -> Board
insert (p, val) (Board count ps) =
  let newPs = M.insert p val ps
  in case M.lookup p ps of
    Nothing -> Board (count+1) newPs
    Just _  -> Board count newPs

bLookup :: Board -> Point -> Maybe Int
bLookup (Board _ ps) p = M.lookup p ps

showBoardPoint :: Board -> Int -> Int -> String
showBoardPoint b x y =
  case bLookup b (x,y) of
    Nothing -> "   "
    Just x -> " " ++ show x ++ " "

showBoardRow :: Board -> Int -> String
showBoardRow board x =
     mconcat (fmap (showBoardPoint board x) [1..3]) ++ "|"
  ++ mconcat (fmap (showBoardPoint board x) [4..6]) ++ "|"
  ++ mconcat (fmap (showBoardPoint board x) [7..9]) ++ "\n"

showBoard :: Board -> String
showBoard board =
     mconcat (fmap (showBoardRow board) [1..3])
  ++ replicate 29 '-' ++ "\n"
  ++ mconcat (fmap (showBoardRow board) [4..6])
  ++ replicate 29 '-' ++ "\n"
  ++ mconcat (fmap (showBoardRow board) [7..9])

mkBoard :: [(Point, Int)] -> Board
mkBoard = foldr insert empty

emptyKeys :: Board -> [Point]
emptyKeys board = filter notUsed allPoints
  where
    usedKeys = M.keys (points board)
    notUsed :: Point -> Bool
    notUsed x = x `notElem` usedKeys

allPoints :: [Point]
allPoints = [(m,n) | m <- [1..9], n <- [1..9]]

values :: Set Int
values = foldr S.insert S.empty [1..9]

findOpen :: (Point -> [Point]) -> Point -> Board -> Set Int
findOpen f p board =
  let keys = f p
      vals = mapMaybe (bLookup board) keys
  in foldr S.delete values vals

openRowVals :: Point -> Board -> Set Int
openRowVals = findOpen (\(x,y) -> fmap ((,) x) [1..9])

openColVals :: Point -> Board -> Set Int
openColVals = findOpen (\(_,y) -> fmap (\x -> (x,y)) [1..9])

findRange :: Int -> [Int]
findRange n
  | 1 <= n && n <= 3 = [1..3]
  | 4 <= n && n <= 6 = [4..6]
  | otherwise        = [7..9]

openSquareVals :: Point -> Board -> Set Int
openSquareVals = findOpen squarePoints

openVals :: Board -> Point -> Set Int
openVals board p =
  let rowVals = openRowVals p board
      colVals = openColVals p board
      sqVals  = openSquareVals p board
  in S.intersection rowVals (S.intersection colVals sqVals)

squarePoints :: Point -> [Point]
squarePoints (x,y) = [(m,n) | m <- findRange x, n <- findRange y]

allOpen :: Board -> [(Point, Set Int)]
allOpen board = sortBy comparePoints $ fmap keyCandidates (emptyKeys board)
  where
    keyCandidates p = (p, openVals board p)

mostConstrained :: Board -> (Point, Set Int)
mostConstrained = head . allOpen

comparePoints :: (Point, Set Int) -> (Point, Set Int) -> Ordering
comparePoints (_,x) (_,y) = compare (S.size x) (S.size y)

backtrack :: Int -> c
          -> (Board -> Int -> c -> Bool)
          -> (Board -> Int -> c -> Board)
          -> (Board -> Int -> c -> [Board])
          -> Board -> [Board]
backtrack k inp solves process mkCandidates xs =
  if solves xs k inp
  then [process xs k inp]
  else
    let candidates = mkCandidates xs (k+1) inp
        newBacktrack = backtrack (k+1) inp solves process mkCandidates
    in mconcat (fmap newBacktrack candidates)

done :: Board -> Int -> c -> Bool
done (Board 81 _) _ _ = True
done _            _ _ = False

constB :: Board -> Int -> c -> Board
constB b _ _ = b

moves :: Board -> Int -> c -> [Board]
moves board _ _ =
  let (p, mvs) = mostConstrained board
  in fmap (addPossible p) (S.toAscList mvs)
  where
    addPossible :: Point -> Int -> Board
    addPossible p' n = insert (p',n) board

pruned :: [Int] -> Int -> Int -> [Int]
pruned xs k n =
  let possibles = S.fromList [1..n]
      candidates = foldr S.delete possibles xs
  in S.toAscList $ S.delete k candidates

sudoku :: Board -> Board
sudoku board = head $ backtrack 0 0 done constB moves board

hardGame :: Board
hardGame = mkBoard
  [ ((1,8), 1) , ((1,9), 2)
  , ((2,5), 3) , ((2,6), 5)
  , ((3,4), 6) , ((3,8), 7)
  , ((4,1), 7) , ((4,7), 3)
  , ((5,4), 4) , ((5,7), 8)
  , ((6,1), 1)
  , ((7,4), 1) , ((7,5),2 )
  , ((8,2), 8) , ((8,8), 4)
  , ((9,2), 5) , ((9,7), 6) ]

-- IO

toInt :: Char -> Int
toInt c = ord c - ord '0'

readLine :: String -> Maybe (Point, Int)
readLine (a:' ':b:' ':c:_)
  | all isDigit [a,b,c] = Just ((toInt a, toInt b), toInt c)
  | otherwise           = Nothing
readLine _ = Nothing

readLines :: String -> [(Point, Int)]
readLines = mapMaybe readLine . lines

readBoard :: String -> Board
readBoard = foldr insert empty . readLines

getBoard :: String -> IO Board
getBoard filename = do
  content <- readFile filename
  return $ readBoard content
