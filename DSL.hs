module DSL where

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import System.IO.Unsafe

import Types

-- AST Generation

instance Num ExprAST where
    (+) a b = BinOpAST AddOp a b
    (-) a b = BinOpAST AddOp a (UnOpAST NegateOp b)
    (*) a b = BinOpAST MulOp a b
    abs a = error "ABS Unimplemented"
    signum = error "Signum Unimplemented"
    fromInteger = error "fromInteger Unimplemented"

instance Fractional ExprAST where
    (/) a b = BinOpAST DivOp a b
    recip = error "recip Unimplemented"
    fromRational = error "fromRational Unimplemented"

hc :: ExprAST -> ExprAST
hc a = UnOpAST ConjugateOp a

-- Functions that make up the DSL itself

pushTAC :: TAC -> TACM ()
pushTAC x = do
    a <- get
    put $ a {tacAssembly = tacAssembly a ++ [x]}
    return ()

getFreshTmpVar :: TACM (Ref)
getFreshTmpVar = do
    a <- get
    let id = varID a
    put $ a {varID = id + 1}
    return $ Ref ("t" ++ (show id))

ast2tac :: ExprAST -> TACM Ref
ast2tac (BinOpAST x l r) = do
    dest <- getFreshTmpVar
    ll <- ast2tac l
    rr <- ast2tac r
    pushTAC $ (TAC dest (BinOpTac x ll rr))
    return dest

ast2tac (UnOpAST x a) = do
    dest <- getFreshTmpVar
    aa <- ast2tac a
    pushTAC $ (TAC dest (UnOpTac x aa))
    return dest

ast2tac (RefAST x) = do
    return x

getRef :: Invar -> RefType -> String -> TACM (ExprAST)
getRef i rt s = do
    db <- get
    let r = Ref s
    let info = MetaInfo {
        ref = r,
        invar = i,
        typeInfo = rt
        }
    let m = M.insert r info (meta db)
    put $ db {meta = m}
    return $ RefAST r

emptyTACDB = TACDB {tacAssembly = [], varID = 0, meta = M.empty}

-- Pretty Printing

ref2n (Ref n) = n

binOp2Symbol (AddOp) = "+"
binOp2Symbol (MulOp) = "*"
binOp2Symbol (DivOp) = "/"

unOp2Name (ConjugateOp) = "h.c."
unOp2Name (NegateOp) = "-"
unOp2Name (SqrtOp) = "sqrt"
unOp2Name (TraceOp) = "tr"

dumpTACInsn :: TAC -> [Char]
dumpTACInsn (TAC d (BinOpTac op l r)) = ref2n d ++ " := " ++ ref2n l ++ " " ++ binOp2Symbol op ++ " " ++ ref2n r
dumpTACInsn (TAC d (UnOpTac op arg)) = ref2n d ++ " := " ++ unOp2Name op ++ " " ++ ref2n arg

dumpTACListing :: [TAC] -> [Char]
dumpTACListing x = intercalate "\n" (map dumpTACInsn x)

