module Lib where

import qualified Parser as P
import Emiter (emitC)
-- import Data.Either (fromRight)
eval :: String -> String
eval a = 
    let 
      result = P.parseExpr a
    in
      case result of
        Right a ->
          emitC a 
        Left b ->
          show b

run :: IO ()
run = do
    putStrLn "Function/Variable Creation"
    putStrLn "--------------------------"
    putStrLn $ eval "plus a c = a + c"
    putStrLn $ fromRight "parse error" $ P.parseExpr "plus a c = a + c"
    putStrLn $ eval "test = 10"
    putStrLn $ fromRight "parse error" $ P.parseExpr "test = 10"
    putStrLn "--------------------------\n"

    putStrLn "Function Call"
    putStrLn "--------------------------"
    putStrLn $ eval "(plus (1) (2))"
    putStrLn $ fromRight "parse error" $ P.parseExpr  "(plus (1) (2))"
    putStrLn "--------------------------\n"

    putStrLn "If Else Statement"
    putStrLn "--------------------------"
    putStrLn $ eval "isBiggerThanFive a = if a > 5 then 1 else 0"
    putStrLn $ fromRight "parse error" $ P.parseExpr  "isBiggerThanFive a = if a > 5 then 1 else 0"
    putStrLn "--------------------------\n"

    putStrLn "Recursive"
    putStrLn "--------------------------"
    putStrLn $ eval "sum a = if a > 0 then a + (sum (a - 1) ) else 0"
    putStrLn $ fromRight "parse error" $ P.parseExpr  "sum a = if a > 0 then (sum (a - 1) ) else 0"
    putStrLn "--------------------------\n"

    putStrLn "Optimization"
    putStrLn "--------------------------"
    putStrLn $ eval "test = 10 * 10"
    putStrLn $ eval "test = 10 * 1"
    putStrLn $ eval "test = 10 + 10"
    putStrLn $ eval "test = 10 + 0"
    putStrLn "--------------------------\n"


fromRight ::(Show b) => String -> Either a b -> String
fromRight defaultVal eitherVal = 
  case eitherVal of 
    Right a -> show a
    Left _ -> defaultVal
