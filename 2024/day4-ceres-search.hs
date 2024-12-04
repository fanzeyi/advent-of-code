import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)

parse :: String -> [[Char]]
parse = lines

data Direction = Right | Left | Bottom | Top | DownRight | DownLeft | UpRight | UpLeft deriving (Enum, Eq)

needle = "XMAS"

nextCoordinate_ :: Direction -> (Int, Int) -> (Int, Int)
nextCoordinate_ Main.Right (x, y) = (x, y + 1)
nextCoordinate_ Main.Left (x, y) = (x, y - 1)
nextCoordinate_ Bottom (x, y) = (x + 1, y)
nextCoordinate_ Top (x, y) = (x - 1, y)
nextCoordinate_ DownRight (x, y) = (x + 1, y + 1)
nextCoordinate_ DownLeft (x, y) = (x + 1, y - 1)
nextCoordinate_ UpRight (x, y) = (x - 1, y + 1)
nextCoordinate_ UpLeft (x, y) = (x - 1, y - 1)

ensureCoordinate :: [[Char]] -> (Int, Int) -> Maybe (Int, Int)
ensureCoordinate matrix (x, y)
  | x < 0 = Nothing
  | y < 0 = Nothing
  | x >= xLength = Nothing
  | y >= yLength = Nothing
  | otherwise = Just (x, y)
  where
    xLength = length matrix
    yLength = length $ matrix !! x

nextCoordinate :: [[Char]] -> Direction -> (Int, Int) -> Maybe (Int, Int)
nextCoordinate matrix direction coordinate =
  let next = nextCoordinate_ direction coordinate
   in ensureCoordinate matrix next

search2_ :: [[Char]] -> [Char] -> Direction -> (Int, Int) -> Bool
search2_ matrix [] _ _ = True
search2_ matrix (h : hs) direction (x, y)
  | h == (matrix !! x !! y) =
      let maybeNext = nextCoordinate matrix direction (x, y)
       in case maybeNext of
            Nothing -> hs == []
            Just next -> search2_ matrix hs direction next
  | otherwise = False

search2 :: [[Char]] -> Direction -> (Int, Int) -> Bool
search2 matrix = search2_ matrix needle

--   traceShow ("Searching for " ++ needle ++ " at " ++ (show (x, y)) ++ " = " ++ [(matrix !! x !! y)]) $ False
-- foldl (\) 0 needle

nextSearch :: [[Char]] -> (Int, Int) -> Maybe (Int, Int)
nextSearch matrix (x, y)
  | x == xLength = Nothing
  | y == yLength - 1 && x == xLength - 1 = Nothing
  | y == yLength - 1 = Just (x + 1, 0)
  | otherwise = Just (x, y + 1)
  where
    xLength = length matrix
    yLength = length $ matrix !! x

search_ :: [[Char]] -> Direction -> (Int, Int) -> Int
search_ matrix direction coordinate =
  let nextCoordinate = nextSearch matrix coordinate
      maybeNext = fmap (search_ matrix direction) nextCoordinate
      next = fromMaybe 0 maybeNext
   in case search2 matrix direction coordinate of
        True -> 1 + next
        False -> next

search :: [[Char]] -> Direction -> Int
search matrix direction = search_ matrix direction (0, 0)

solveFirst :: [[Char]] -> Int
solveFirst matrix =
  sum $
    map (search matrix) $
      [ Main.Right,
        Main.Left,
        Bottom,
        Top,
        DownRight,
        DownLeft,
        UpRight,
        UpLeft
      ]

needle2 = "MAS"

opposite :: Direction -> Direction
opposite UpLeft = DownRight
opposite UpRight = DownLeft
opposite DownLeft = UpRight
opposite DownRight = UpLeft

checkCross' :: [[Char]] -> (Int, Int) -> Direction -> Bool
checkCross' matrix coordinate direction =
  let oppositeDirection = opposite direction
      maybeStart = nextCoordinate matrix oppositeDirection coordinate
   in case fmap (search2_ matrix needle2 direction) maybeStart of
        Just True -> True
        _ -> False

crossDirections = [DownRight, DownLeft, UpRight, UpLeft]

checkCross :: [[Char]] -> (Int, Int) -> Bool
checkCross matrix coordinate = (length $ filter (== True) $ map (checkCross' matrix coordinate) crossDirections) >= 2

searchSecond :: [[Char]] -> (Int, Int) -> Int
searchSecond matrix coordinate =
  let nextCoordinate = nextSearch matrix coordinate
      maybeNext = fmap (searchSecond matrix) nextCoordinate
      next = fromMaybe 0 maybeNext
   in case checkCross matrix coordinate of
        True -> 1 + next
        False -> next

solveSecond :: [[Char]] -> Int
solveSecond matrix = searchSecond matrix (0, 0)

main = interact $ show . solveSecond . parse
