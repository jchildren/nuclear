module Language.Nuclear.Eval
  ( Value(..)
  , TermEnv
  , Interpreter
  , runEval
  , emptyTermEnv
  ) where

import           Control.Monad.Identity
import qualified Data.Map.Strict         as Map

import           Language.Nuclear.Syntax

data Value
  = VInt Int
  | VBool Bool
  | VClosure String
             Expr
             TermEnv
  deriving (Show)

type TermEnv = Map.Map String Value

type Interpreter t = Identity t

emptyTermEnv :: TermEnv
emptyTermEnv = Map.empty

eval :: TermEnv -> Expr -> Interpreter Value
eval env expr =
  case expr of
    Lit (LInt k) -> return $ VInt k
    Lit (LBool k) -> return $ VBool k
    Var x -> do
      let Just v = Map.lookup x env
      return v
    Op op a b -> do
      VInt a' <- eval env a
      VInt b' <- eval env b
      return $ binop op a' b'
    Lam x body -> return (VClosure x body env)
    App fun arg -> do
      VClosure x body clo <- eval env fun
      argv <- eval env arg
      let nenv = Map.insert x argv clo
      eval nenv body
    Let x e body -> do
      e' <- eval env e
      let nenv = Map.insert x e' env
      eval nenv body
    If cond tr fl -> do
      VBool br <- eval env cond
      if br
        then eval env tr
        else eval env fl
    Fix e -> eval env (App e (Fix e))

binop :: BinOp -> Int -> Int -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b

runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
  let res = runIdentity (eval env ex)
  in (res, Map.insert nm res env)
