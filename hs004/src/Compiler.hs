module Compiler (Token (..), Expr (..), tokenize, parse, eval) where

import Data.Char (isDigit, isSpace)

data Token
  = TInt Int
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TLParen
  | TRParen
  deriving (Show, Eq)

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  deriving (Show, Eq)

-- Lexer

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | isDigit c =
      let (digits, rest) = span isDigit (c : cs)
       in (TInt (read digits) :) <$> tokenize rest
  | c == '+' = (TPlus :) <$> tokenize cs
  | c == '-' = (TMinus :) <$> tokenize cs
  | c == '*' = (TStar :) <$> tokenize cs
  | c == '/' = (TSlash :) <$> tokenize cs
  | c == '(' = (TLParen :) <$> tokenize cs
  | c == ')' = (TRParen :) <$> tokenize cs
  | otherwise = Left ("unexpected character: " ++ [c])

-- Parser
--
-- expr   ::= term   (('+' | '-') term)*
-- term   ::= factor (('*' | '/') factor)*
-- factor ::= INT | '(' expr ')' | '-' factor

type ParseResult a = Either String (a, [Token])

parse :: [Token] -> Either String Expr
parse tokens = do
  (expr, rest) <- parseExpr tokens
  case rest of
    [] -> Right expr
    (t : _) -> Left ("unexpected token: " ++ show t)

parseExpr :: [Token] -> ParseResult Expr
parseExpr tokens = do
  (left, rest) <- parseTerm tokens
  parseExprRest left rest

parseExprRest :: Expr -> [Token] -> ParseResult Expr
parseExprRest left (TPlus : rest) = do
  (right, rest') <- parseTerm rest
  parseExprRest (Add left right) rest'
parseExprRest left (TMinus : rest) = do
  (right, rest') <- parseTerm rest
  parseExprRest (Sub left right) rest'
parseExprRest left rest = Right (left, rest)

parseTerm :: [Token] -> ParseResult Expr
parseTerm tokens = do
  (left, rest) <- parseFactor tokens
  parseTermRest left rest

parseTermRest :: Expr -> [Token] -> ParseResult Expr
parseTermRest left (TStar : rest) = do
  (right, rest') <- parseFactor rest
  parseTermRest (Mul left right) rest'
parseTermRest left (TSlash : rest) = do
  (right, rest') <- parseFactor rest
  parseTermRest (Div left right) rest'
parseTermRest left rest = Right (left, rest)

parseFactor :: [Token] -> ParseResult Expr
parseFactor (TInt n : rest) = Right (Lit n, rest)
parseFactor (TLParen : rest) = do
  (expr, rest') <- parseExpr rest
  case rest' of
    (TRParen : rest'') -> Right (expr, rest'')
    _ -> Left "expected closing parenthesis"
parseFactor (TMinus : rest) = do
  (expr, rest') <- parseFactor rest
  Right (Neg expr, rest')
parseFactor [] = Left "unexpected end of input"
parseFactor (t : _) = Left ("unexpected token: " ++ show t)

-- Evaluator

eval :: Expr -> Either String Int
eval (Lit n) = Right n
eval (Add l r) = (+) <$> eval l <*> eval r
eval (Sub l r) = (-) <$> eval l <*> eval r
eval (Mul l r) = (*) <$> eval l <*> eval r
eval (Div l r) = do
  lv <- eval l
  rv <- eval r
  if rv == 0 then Left "division by zero" else Right (lv `div` rv)
eval (Neg e) = negate <$> eval e
