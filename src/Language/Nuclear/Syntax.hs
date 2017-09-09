module Language.Nuclear.Syntax
  ( Name
  , Expr(..)
  , BinOp(..)
  , Lit(..)
  , Program(..)
  , Decl
  ) where

type Name = String

data Expr
  = Var Name
  | Lam Name
        Expr
  | App Expr
        Expr
  | Let Name
        Expr
        Expr
  | Lit Lit
  | If Expr
       Expr
       Expr
  | Fix Expr
  | Op BinOp
       Expr
       Expr
  deriving (Eq, Show, Ord)

data BinOp
  = Add
  | Sub
  | Mul
  | Eql
  deriving (Eq, Ord, Show)

data Lit
  = LBool Bool
  | LInt Int
  | LDouble Double
  deriving (Show, Eq, Ord)

data Program =
  Program [Decl]
          Expr
  deriving (Eq)

newtype Decl =
  Decl (String, Expr)
  deriving (Show, Eq, Ord)
