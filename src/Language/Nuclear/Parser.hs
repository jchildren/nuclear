{-# LANGUAGE OverloadedStrings #-}

module Language.Nuclear.Parser
  ( runParseModule
  ) where

import qualified Data.Text.Lazy             as TL
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

import           Language.Nuclear.Syntax

type Parser = Parsec Decl TL.Text

-- Lexer helpers
{-# INLINE sc #-}
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

{-# INLINE symbol #-}
symbol :: TL.Text -> Parser TL.Text
symbol = L.symbol sc

-- Combinators
{-# INLINE parens #-}
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Primitive data types
{-# INLINE double #-}
double :: Parser Double
double = lexeme L.float

{-# INLINE integer #-}
integer :: Parser Int
integer = lexeme L.decimal

{-# INLINE boolean #-}
boolean :: Parser Bool
boolean = True <$ reserved "True" <|> False <$ reserved "False"

-- Reserved tokens and identifiers
{-# INLINE reserved #-}
reserved :: TL.Text -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

{-# INLINE rws #-}
rws :: [String]
rws = ["True", "False", "let"]

{-# INLINE identifier #-}
identifier :: Parser Name
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

-- Expressions
expr :: Parser Expr
expr = makeExprParser term operators

operators :: [[Operator Parser Expr]]
operators =
  [ [InfixL (Op Mul <$ symbol "*")]
  , [InfixL (Op Add <$ symbol "+")]
  , [InfixL (Op Sub <$ symbol "-")]
  , [InfixL (Op Eql <$ symbol "==")]
  ]

lambda :: Parser Expr
lambda = do
  symbol "\\"
  arg <- identifier
  symbol "->"
  body <- expr
  return $ Lam arg body

term :: Parser Expr
term =
  aexp >>= \x -> (some aexp >>= \xs -> return (foldl App x xs)) <|> return x

aexp :: Parser Expr
aexp =
  parens expr <|> Var <$> identifier <|> lambda <|> Lit . LDouble <$> double <|>
  Lit . LInt <$> integer <|>
  Lit . LBool <$> boolean

type Binding = (String, Expr)

signature :: Parser (Name, [Name], Expr)
signature = do
  name <- identifier
  args <- many identifier
  symbol "="
  body <- expr
  return (name, args, body)

letdecl :: Parser Binding
letdecl = do
  reserved "let"
  (name, args, body) <- signature
  return (name, foldr Lam body args)

letrecdecl :: Parser Binding
letrecdecl = do
  reserved "let"
  reserved "rec"
  (name, args, body) <- signature
  return (name, Fix $ foldr Lam body (name : args))

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> val

top :: Parser Binding
top = do
  x <- decl
  optional (symbol ";")
  return x

modl :: Parser [Binding]
modl = many top

runParseModule ::
     String -> TL.Text -> Either (ParseError (Token TL.Text) Decl) [Binding]
runParseModule = runParser modl
