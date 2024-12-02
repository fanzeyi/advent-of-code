import Data.List (sort)
import Data.Map qualified as Map
import Debug.Trace (traceShow)

dbg :: (Show a) => a -> a
dbg x = dbg2 ("Debug: " ++ show x) x

dbg2 :: (Show a) => [Char] -> a -> a
dbg2 y x = traceShow ("Debug: " ++ y ++ show x) x

solveFirst :: [(Int, Int)] -> Int
solveFirst input =
  let first = sort $ map fst input
      second = sort $ map snd input
   in sum $ map (\(a, b) -> abs $ a - b) $ zip first second

buildFrequencyMap :: [Int] -> Map.Map Int Int
buildFrequencyMap input = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty input

solveSecond :: [(Int, Int)] -> Int
solveSecond input =
  let first = sort $ map fst input
      second = sort $ map snd input
      secondFreq = buildFrequencyMap second
   in sum $ map (\x -> x * (Map.findWithDefault 0 x secondFreq)) first

parse :: String -> [(Int, Int)]
parse input = map parseLine nonEmptyLines
  where
    nonEmptyLines = filter (not . null) $ lines input
    parseLine line =
      let [a, b] = map read $ words line
       in (a, b)

main :: IO ()
main = interact $ show . solveSecond . parse
