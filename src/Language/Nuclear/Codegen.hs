{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Nuclear.Codegen where

import           Data.Function
import           Data.List
import qualified Data.Map                        as Map
import           Data.Monoid
import           Data.String

import           Control.Applicative
import           Control.Monad.State

import qualified LLVM.AST                        as AST
import           LLVM.AST.Global
import           LLVM.Prelude

import qualified LLVM.AST.Attribute              as A
import qualified LLVM.AST.CallingConvention      as CC
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Linkage                as L

double :: AST.Type
double = AST.FloatingPointType AST.DoubleFP

type SymbolTable = [(ShortByteString, AST.Operand)]

data CodegenState = CodegenState
  { currentBlock :: AST.Name
  , blocks       :: Map.Map AST.Name BlockState
  , symtab       :: SymbolTable
  , blockCount   :: Int
  , count        :: Word
  , names        :: Names
  } deriving (Show)

data BlockState = BlockState
  { idx   :: Int
  , stack :: [AST.Named AST.Instruction]
  , term  :: Maybe (AST.Named AST.Terminator)
  } deriving (Show)

newtype Codegen a = Codegen
  { runCodegen :: State CodegenState a
  } deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a =
  LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString -> AST.Module
emptyModule label = AST.defaultModule {AST.moduleName = label}

addDefn :: AST.Definition -> LLVM ()
addDefn d = do
  defs <- gets AST.moduleDefinitions
  modify $ \s -> s {AST.moduleDefinitions = defs ++ [d]}

define ::
     AST.Type
  -> ShortByteString
  -> [(AST.Type, AST.Name)]
  -> [BasicBlock]
  -> LLVM ()
define retty label argtys body =
  addDefn $
  AST.GlobalDefinition $
  AST.functionDefaults
  { name = AST.Name label
  , parameters = (params, False)
  , returnType = retty
  , basicBlocks = body
  }
  where
    params = [Parameter ty nm [] | (ty, nm) <- argtys]

external :: AST.Type -> ShortByteString -> [(AST.Type, AST.Name)] -> LLVM ()
external retty label argtys =
  addDefn $
  AST.GlobalDefinition $
  AST.functionDefaults
  { name = AST.Name label
  , linkage = L.External
  , parameters = (params, False)
  , returnType = retty
  , basicBlocks = []
  }
  where
    params = [Parameter ty nm [] | (ty, nm) <- argtys]

-- Blocks
entry :: Codegen AST.Name
entry = gets currentBlock

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

addBlock :: ShortByteString -> Codegen AST.Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s ->
    s
    { blocks = Map.insert (AST.Name qname) new bls
    , blockCount = ix + 1
    , names = supply
    }
  return (AST.Name qname)

setBlock :: AST.Name -> Codegen AST.Name
setBlock bname = do
  modify $ \s -> s {currentBlock = bname}
  return bname

getBlock :: Codegen AST.Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s {blocks = Map.insert active new (blocks s)}

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "No such block: " ++ show c

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (AST.Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing  = error $ "Block has no terminator: " ++ show l

sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

entryBlockName :: ShortByteString
entryBlockName = fromString "entry"

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (AST.Name entryBlockName) Map.empty [] 1 0 Map.empty

-- Names
fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s {count = 1 + i}
  return $ i + 1

type Names = Map.Map ShortByteString Int

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm <> fromString (show ix), Map.insert nm (ix + 1) ns)

local :: AST.Name -> AST.Operand
local = AST.LocalReference double

externf :: AST.Name -> AST.Operand
externf = AST.ConstantOperand . C.GlobalReference double

-- assignment
assign :: ShortByteString -> AST.Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s {symtab = (var, x) : lcls}

getvar :: ShortByteString -> Codegen AST.Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: AST.Instruction -> Codegen AST.Operand
instr ins = do
  n <- fresh
  let ref = AST.UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = (ref AST.:= ins) : i})
  return $ local ref

terminator :: AST.Named AST.Terminator -> Codegen (AST.Named AST.Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk {term = Just trm})
  return trm

-- operators
fadd :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fadd a b = instr $ AST.FAdd AST.NoFastMathFlags a b []

fsub :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fsub a b = instr $ AST.FSub AST.NoFastMathFlags a b []

fmul :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fmul a b = instr $ AST.FMul AST.NoFastMathFlags a b []

fdiv :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fdiv a b = instr $ AST.FDiv AST.NoFastMathFlags a b []

cons :: C.Constant -> AST.Operand
cons = AST.ConstantOperand

toArgs :: [AST.Operand] -> [(AST.Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- control flow
br :: AST.Name -> Codegen (AST.Named AST.Terminator)
br val = terminator $ AST.Do $ AST.Br val []

cbr :: AST.Operand -> AST.Name -> AST.Name -> Codegen (AST.Named AST.Terminator)
cbr cond tr fl = terminator $ AST.Do $ AST.CondBr cond tr fl []

ret :: AST.Operand -> Codegen (AST.Named AST.Terminator)
ret val = terminator $ AST.Do $ AST.Ret (Just val) []

-- effects
call :: AST.Operand -> [AST.Operand] -> Codegen AST.Operand
call fn args = instr $ AST.Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: AST.Type -> Codegen AST.Operand
alloca ty = instr $ AST.Alloca ty Nothing 0 []

store :: AST.Operand -> AST.Operand -> Codegen AST.Operand
store ptr val = instr $ AST.Store False ptr val Nothing 0 []

load :: AST.Operand -> Codegen AST.Operand
load ptr = instr $ AST.Load False ptr Nothing 0 []
