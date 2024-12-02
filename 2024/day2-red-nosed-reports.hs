import Debug.Trace (traceShow)

dbg :: (Show a) => a -> a
dbg x = dbg2 ("Debug: " ++ show x) x

dbg2 :: (Show a) => [Char] -> a -> a
dbg2 y x = traceShow ("Debug: " ++ y ++ show x) x

parse :: String -> [[Int]]
parse = map (map read . words) . lines

data Direction = Increasing | Decreasing deriving (Enum, Eq)

determineDirection :: [Int] -> Direction
determineDirection (x : y : xs) = if x < y then Increasing else Decreasing

safePair :: Direction -> (Int, Int) -> Bool
safePair Increasing (a, b) = safePair_ $ b - a
safePair Decreasing (a, b) = safePair_ $ a - b

safePair_ :: Int -> Bool
safePair_ x = x <= 3 && x >= 1

determineSafety :: [Int] -> Bool
determineSafety record =
  let direction = determineDirection record
      pairs = zip record (tail record)
   in all (safePair direction) pairs

solveFirst :: [[Int]] -> Int
solveFirst a = length $ filter determineSafety a

data Outcome = Safe | Fixed | Unsafe deriving (Enum, Eq, Show)

safePair2 :: Direction -> (Int, Int, Int) -> Outcome
safePair2 direction (a, b, c)
  | safePair direction (a, b) = Safe
  | safePair direction (a, c) = Fixed
  | otherwise = Unsafe

determineSafety2 :: [Int] -> Bool
determineSafety2 record =
  let direction = determineDirection record
      pairs = zip3 record (tail record) (tail $ tail record)
      outcome = map (safePair2 direction) pairs
      fixed = length $ filter (== Fixed) outcome
   in fixed <= 1 && all (/= Unsafe) outcome

solveSecond :: [[Int]] -> Int
solveSecond a = length $ filter determineSafety2 a

main :: IO ()
main = interact $ show . solveSecond . parse
