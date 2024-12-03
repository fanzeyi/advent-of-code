import Control.Applicative ((<*))
import Data.Function ((&))
import Data.Maybe (catMaybes, isJust)
import Debug.Trace (traceShow)
import Text.Parsec hiding (parse)
import Text.Parsec qualified as Parsec
import Text.Parsec.String

dbg :: (Show a) => a -> a
dbg x = dbg2 "" x

dbg2 :: (Show a) => [Char] -> a -> a
dbg2 label x = traceShow ("Debug: " ++ label ++ " " ++ show x) x

data Call = Mul Int Int | Do | Dont deriving (Show, Eq)

mulParser :: Parser (Maybe Call)
mulParser = do
  string "mul("
  lhs <- many1 digit
  char ','
  rhs <- many1 digit
  char ')'
  return $ Just $ Mul (read lhs) (read rhs)

doParser :: Parser (Maybe Call)
doParser = string "do()" *> return (Just Do)

dontParser :: Parser (Maybe Call)
dontParser = string "don't()" *> return (Just Dont)

mainParser :: Parser [Maybe Call]
mainParser = many (try mulParser <|> try doParser <|> try dontParser <|> anyChar *> return Nothing)

parse :: String -> [Call]
parse input = case Parsec.parse mainParser "" input of
  Left _ -> []
  Right results -> catMaybes results

flattenCalls_ :: Bool -> [Call] -> [Call]
flattenCalls_ _ [] = []
flattenCalls_ a (Do : xs) = flattenCalls_ True xs
flattenCalls_ a (Dont : xs) = flattenCalls_ False xs
flattenCalls_ True (x : xs) = x : flattenCalls_ True xs
flattenCalls_ False (x : xs) = flattenCalls_ False xs

flattenCalls :: [Call] -> [Call]
flattenCalls = flattenCalls_ True

calculateCalls :: String -> Int
calculateCalls input =
  let calls = parse input
   in sum $ map (\(Mul a b) -> a * b) $ flattenCalls $ calls

solve :: String -> Int
solve input = calculateCalls $ filter (/= '\n') input

main :: IO ()
main = interact $ show . calculateCalls
