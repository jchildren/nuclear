module Language.Nuclear.Emit where

import           Control.Monad.Except
import qualified Data.Map                        as Map
import           Data.String

import           LLVM.Context
import           LLVM.Module

import qualified LLVM.AST                        as AST
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.Float                  as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import           LLVM.Prelude

import           Language.Nuclear.Codegen
import           Language.Nuclear.Syntax

one = cons $ C.Float (F.Double 1.0)

zero = cons $ C.Float (F.Double 0.0)

false = zero

true = one

codegenTop :: Expr -> LLVM ()
{-
codegenTop (Let name args body) = define double (fromString name) fnargs bls
  where
    fnargs = toSig args
    bls =
      createBlocks $
      execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM_ args $ \a -> do
          store var (local (AST.Name a))
          assign a var
        cgen body >>= ret
-}
codegenTop (Lam x body) = define double (fromString "lambda") fnargs bls
  where
    argName = fromString $ show x
    fnargs = toSig argName
    bls =
      createBlocks $
      execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        var <- alloca double
        store var (local (AST.Name argName))
        assign argName var
        cgen body >>= ret
codegenTop exp = define double (fromString "main") [] blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        cgen exp >>= ret

toSig :: ShortByteString -> [(AST.Type, AST.Name)]
toSig x = [(double, AST.Name x)]

cgen :: Expr -> Codegen AST.Operand
cgen (Lit (LDouble n)) = return $ cons $ C.Float (F.Double n)
cgen (Var name) = getvar (fromString name) >>= load
cgen (Op op a b) =
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
--cgen (App fn args) = do
--  largs <- mapM cgen [args]
--  call (externf (AST.Name fn)) largs
cgen (Lam x body) = undefined
cgen (Let x e body) = undefined
cgen (Fix e) = undefined
cgen (If cond tr fl) = do
  ifthen <- addBlock $ fromString "if.then"
  ifelse <- addBlock $ fromString "if.else"
  ifexit <- addBlock $ fromString "if.exit"
  -- entry
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifthen ifelse
  -- if.then
  setBlock ifthen
  trval <- cgen tr
  br ifexit
  ifthen <- getBlock
  -- if.else
  setBlock ifelse
  flval <- cgen fl
  br ifexit
  ifelse <- getBlock
  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]
cgen x = error (show x)

binops = Map.fromList [(Add, fadd), (Sub, fsub), (Mul, fmul)]

codegen :: AST.Module -> [Expr] -> IO AST.Module
codegen mod fns =
  withContext $ \context ->
    withModuleFromAST context newast $ \m -> do
      llstr <- moduleLLVMAssembly m
      print llstr
      return newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM mod modn

initModule :: AST.Module
initModule = emptyModule (fromString "my not so cool jit")
