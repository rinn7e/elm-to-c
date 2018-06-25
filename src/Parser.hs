module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+","*","-","/",";","=",",","<",">","|",":"]
    names = ["def","extern","if","then","else","in","for"
            ,"binary", "unary", "var"]
    style = emptyDef {
               Tok.commentLine = "#"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

integer    = Tok.integer lexer
float      = Tok.float lexer
parens     = Tok.parens lexer
commaSep   = Tok.commaSep lexer
semiSep    = Tok.semiSep lexer
identifier = Tok.identifier lexer
whitespace = Tok.whiteSpace lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

operator :: Parser String
operator = do
  c <- Tok.opStart emptyDef
  cs <- many $ Tok.opLetter emptyDef
  return (c:cs)


-- Syntax
type Name = String

data Expr
  = Float Double
  | Var String
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | Function Name [Name] Expr
  deriving (Show)
  -- | Function Name [Name] Expr

-- Parser
int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  ls <- many identifier
  reserved "="
  whitespace
  body <- expr
  return $ Function (head ls) (tail ls) body

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

unop = Ex.Prefix (UnaryOp <$> op)

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft

binops = [[binary "=" Ex.AssocLeft]
        ,[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]
        ,[binary "<" Ex.AssocLeft]]

expr :: Parser Expr
expr = Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

factor :: Parser Expr
factor = try function 
      <|> try ifelse
      <|> try floating
      <|> try int
      <|> try variable
      

ifelse :: Parser Expr
ifelse = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

run = 
  parseExpr "isBiggerThanFive a = if a > 5 then 1 else 0"


-- Normal Function
-- plus a b = a + b
-- (Function "plus" ["a", "b"] (BinaryOp "+" (Var "a") (Var "b")))

-- If Else
-- isBiggerThanFive a = if a > 5 then 1 else 0
-- Function "isBiggerThanFive" ["a"] (If (BinaryOp ">" (Var "a") (Float 5.0)) (Float 1.0) (Float 0.0))




