module Types where

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import System.IO.Unsafe

data BinOp = AddOp | MulOp | DivOp  deriving (Show, Ord, Eq)
data UnOp = ConjugateOp | NegateOp | SqrtOp | TraceOp deriving (Show, Ord, Eq)

data Ref = Ref String  deriving (Show, Ord, Eq)

data Abelianness = IsAbelian | IsntAbelian deriving (Show, Ord, Eq)

data TACDest = TacRegDest Ref  deriving (Show, Ord, Eq)

data TACOp =
      BinOpTac BinOp Ref Ref
    | UnOpTac  UnOp Ref  deriving (Show, Ord, Eq)

data TAC = TAC Ref TACOp deriving (Show, Ord, Eq)

data ExprAST =
      RefAST Ref
    | BinOpAST BinOp ExprAST ExprAST
    | UnOpAST UnOp ExprAST
    deriving (Show)

data Invar = IsInvariant | IsntInvariant deriving (Show)

data RefType = RefScalar | RefMatrix deriving (Show)

data MetaInfo = MetaInfo {
    ref :: Ref,
    invar :: Invar,
    typeInfo :: RefType
} deriving (Show)

type MetaDB = M.Map Ref MetaInfo

data TACDB = TACDB {
    tacAssembly :: [TAC] ,
    varID :: Integer ,
    meta :: MetaDB
} deriving (Show)

type TACM = StateT TACDB IO
