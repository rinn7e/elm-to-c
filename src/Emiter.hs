module Emiter where

import Parser as P
import Data.List as DL 

emitC :: Expr -> String
emitC expr = 
  case expr of
    Function name args body ->
      let
        argsWithType = map ((++) "float ") args
          
      in
        "float " ++ name ++ " ( " ++ (DL.intercalate " , " argsWithType) ++ " )" ++ "{\n" ++
          "return " ++ (emitC body) ++ ";\n" ++
        "}\n"

    BinaryOp name (Var var1) (Var var2) ->
      var1 ++ " " ++ name ++ " " ++ var2
    
      
    _ -> 
      "emitC"


test =
  let 
    result = P.parseExpr "plus a b = a + b"
  in
    case result of
      Right a ->
        emitC a 
      Left b ->
        show b



-- Function "plus" ["a","b"] (BinaryOp "+" (Var "a") (Var "b"))
-- float main (float a, float b) {
--   float _a1 = a + b;
--   return _a1;
-- }

-- isBiggerThanFive a = if a > 5 then 1 else 0
-- (If (BinaryOp ">" (Var "a") (Float 5.0)) (Float 1.0) (Float 0.0))

-- float isBiggerThanFive (float a){
--   if ( a > 5 ) {
--     return 1
--   } else {
--     return 0
--   }
-- }