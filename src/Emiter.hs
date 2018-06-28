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
        case body of
          If cond tr fl ->
            "float " ++ name ++ " ( " ++ (DL.intercalate " , " argsWithType) ++ " )" ++ "{\n" ++
              "\tfloat _result;\n" ++
              "\tif ( " ++ (emitC cond)  ++ " ) { \n" ++
                "\t\t_result = ( " ++ (emitC tr) ++ " );\n" ++
              "\t} else {\n" ++
                "\t\t_result = ( " ++ (emitC fl) ++ " );\n" ++
              "\t}\n" ++
              "\treturn _result;\n" ++
              
            "}\n"

          _ -> 

            "float " ++ name ++ " ( " ++ (DL.intercalate " , " argsWithType) ++ " )" ++ "{\n" ++
              "\treturn ( " ++ (emitC body) ++ " );\n" ++ 
            "}\n"

    FunctionCall name args ->
      let
        argValue = map (emitC) args
        
      in
        name ++ " ( " ++ (DL.intercalate ", " argValue) ++ " )"

    BinaryOp name expr1 expr2 ->
      (emitC expr1) ++ " " ++ name ++ " " ++ (emitC expr2)
    
    Float num ->
      show num

    Var string ->
      string
    
    -- If cond tr fl ->
    --   "if ( " ++ (emitC cond)  ++ " ) { \n" ++
    --     "return ( " ++ (emitC tr) ++ " );\n" ++
    --   "else {\n" ++
    --     "return ( " ++ (emitC fl) ++ " );\n" ++
    --   "}\n"
    
      
    _ -> 
      "Cannot emit c code from expression"


-- test =
--   let 
--     result = P.parseExpr "plus a b = a + b"
--   in
--     case result of
--       Right a ->
--         emitC a 
--       Left b ->
--         show b



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