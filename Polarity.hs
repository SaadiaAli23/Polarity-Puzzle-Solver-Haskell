module Polarity (polarity) where

data SignRecord = SignRecord
  { posRows :: [Int]
  , negRows :: [Int]
  , posCols :: [Int]
  , negCols :: [Int]
  } deriving (Show)

type Board = [[Char]]
type Constraints = [(String, [Int])]

getPos :: Board -> Int -> Int -> Char
getPos board r c = board !! r !! c

putAt :: Board -> Int -> Int -> Char -> Board
putAt board r c value =
  take r board ++
  [take c (board !! r) ++ [value] ++ drop (c + 1) (board !! r)] ++
  drop (r + 1) board

adjustments :: [Int] -> Int -> (Int -> Int) -> [Int]
adjustments xs idx f =
  take idx xs ++ [f (xs !! idx)] ++ drop (idx + 1) xs

checkCon :: Constraints -> String -> [Int] -> [Int]
checkCon rules key fallback =
  case lookup key rules of
    Just val -> val
    Nothing -> fallback

nextInd :: Int -> Int -> Int -> Int -> Int
nextInd r c numRows numCols = r * numCols + c + 1

adjustRecord :: SignRecord -> Int -> Int -> Char -> SignRecord
adjustRecord tallies r c pole =
  case pole of
    '+' -> tallies
      { posRows = adjustments (posRows tallies) r (+1)
      , posCols = adjustments (posCols tallies) c (+1)
      }
    '-' -> tallies
      { negRows = adjustments (negRows tallies) r (+1)
      , negCols = adjustments (negCols tallies) c (+1)
      }
    _ -> tallies

isValidCheck :: SignRecord -> Constraints -> Bool
isValidCheck tallies rules =
  let leftRules = checkCon rules "left" []
      rightRules = checkCon rules "right" []
      topRules = checkCon rules "top" []
      bottomRules = checkCon rules "bottom" []
      validRowCheck =
        all (\(val, i) -> let req = leftRules !! i in req == -1 || val <= req) (zip (posRows tallies) [0..]) &&
        all (\(val, i) -> let req = rightRules !! i in req == -1 || val <= req) (zip (negRows tallies) [0..])
      validColCheck =
        all (\(val, j) -> let req = topRules !! j in req == -1 || val <= req) (zip (posCols tallies) [0..]) &&
        all (\(val, j) -> let req = bottomRules !! j in req == -1 || val <= req) (zip (negCols tallies) [0..])
  in validRowCheck && validColCheck

resultCheck :: SignRecord -> Constraints -> Bool
resultCheck tallies rules =
  let leftRules = checkCon rules "left" []
      rightRules = checkCon rules "right" []
      topRules = checkCon rules "top" []
      bottomRules = checkCon rules "bottom" []
      validRowMatch =
        all (\(val, i) -> let req = leftRules !! i in req == -1 || val == req) (zip (posRows tallies) [0..]) &&
        all (\(val, i) -> let req = rightRules !! i in req == -1 || val == req) (zip (negRows tallies) [0..])
      validColMatch =
        all (\(val, j) -> let req = topRules !! j in req == -1 || val == req) (zip (posCols tallies) [0..]) &&
        all (\(val, j) -> let req = bottomRules !! j in req == -1 || val == req) (zip (negCols tallies) [0..])
  in validRowMatch && validColMatch

checkPlacement :: Board -> Int -> Int -> Char -> Int -> Int -> Bool
checkPlacement board r c pole rows cols =
  let adjacent = [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]
  in all (\(nr,nc) -> nr < 0 || nr >= rows || nc < 0 || nc >= cols || getPos board nr nc /= pole) adjacent

placeHorizontal :: Constraints -> Board -> Board -> Int -> SignRecord -> Int -> Int -> Int -> Maybe Board
placeHorizontal rules original answer r tallies c rows cols =
  if c + 1 < cols && getPos answer r c == 'X' && getPos answer r (c+1) == 'X' &&
     (getPos original r c == 'L' && getPos original r (c+1) == 'R')
  then
    let tryPair a b = 
          let temp = putAt (putAt answer r c a) r (c+1) b
              t1 = adjustRecord (adjustRecord tallies r c a) r (c+1) b
          in if checkPlacement temp r c a rows cols &&
                checkPlacement temp r (c+1) b rows cols &&
                isValidCheck t1 rules
             then findSolution (nextInd r c rows cols) rows cols original temp t1 rules
             else Nothing
    in tryPair '+' '-' `orElse` tryPair '-' '+'
  else Nothing

placeVertical :: Board -> Int -> Constraints -> Board -> Int -> SignRecord -> Int -> Int -> Maybe Board
placeVertical original rows rules answer r tallies c cols =
  if r + 1 < rows && getPos answer r c == 'X' && getPos answer (r+1) c == 'X' &&
     (getPos original r c == 'T' && getPos original (r+1) c == 'B')
  then
    let tryPair a b = 
          let temp = putAt (putAt answer r c a) (r+1) c b
              t1 = adjustRecord (adjustRecord tallies r c a) (r+1) c b
          in if checkPlacement temp r c a rows cols &&
                checkPlacement temp (r+1) c b rows cols &&
                isValidCheck t1 rules
             then findSolution (nextInd r c rows cols) rows cols original temp t1 rules
             else Nothing
    in tryPair '+' '-' `orElse` tryPair '-' '+'
  else Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

tryPlacements :: Int -> Constraints -> SignRecord -> Board -> Int -> Int -> Board -> Maybe Board
tryPlacements idx rules tallies answer rows cols original =
  let (r, c) = (idx `div` cols, idx `mod` cols)
  in placeHorizontal rules original answer r tallies c rows cols `orElse`
     placeVertical original rows rules answer r tallies c cols

findSolution :: Int -> Int -> Int -> Board -> Board -> SignRecord -> Constraints -> Maybe Board
findSolution idx rows cols original answer tallies rules
  | idx == rows * cols = if resultCheck tallies rules then Just answer else Nothing
  | getPos answer r c /= 'X' = findSolution (idx + 1) rows cols original answer tallies rules
  | otherwise = tryPlacements idx rules tallies answer rows cols original `orElse`
                findSolution (idx + 1) rows cols original answer tallies rules
  where (r, c) = (idx `div` cols, idx `mod` cols)

polarity :: [String] -> ([Int], [Int], [Int], [Int]) -> [String]
polarity grid (left, right, top, bottom) =
  let rows = length grid
      cols = if null grid then 0 else length (head grid)
      rules = [("left", left), ("right", right), ("top", top), ("bottom", bottom)]
      initialAnswer = replicate rows (replicate cols 'X')
      initialTallies = SignRecord (replicate rows 0) (replicate rows 0) (replicate cols 0) (replicate cols 0)
  in case findSolution 0 rows cols grid initialAnswer initialTallies rules of
      Just sol -> sol
