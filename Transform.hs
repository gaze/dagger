module Transform where

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import System.IO.Unsafe

import Types
import DSL

-- This drives the hashing, cannonicalization, everything
-- Absolutely !MUST! be correct
getBinOpTypeInfo AddOp RefScalar RefScalar = (RefScalar, IsAbelian)
getBinOpTypeInfo AddOp RefMatrix RefMatrix = (RefMatrix, IsAbelian)
getBinOpTypeInfo AddOp _ _ = error "Tried to add a matrix to a scalar"

getBinOpTypeInfo MulOp RefScalar RefMatrix = (RefMatrix, IsAbelian)
getBinOpTypeInfo MulOp RefMatrix RefScalar = (RefMatrix, IsAbelian)
getBinOpTypeInfo MulOp RefMatrix RefMatrix = (RefMatrix, IsntAbelian)
getBinOpTypeInfo MulOp RefScalar RefScalar = (RefScalar, IsAbelian)

getUnOpTypeInfo ConjugateOp x = x
getUnOpTypeInfo NegateOp x = x
getUnOpTypeInfo SqrtOp RefScalar = RefScalar
getUnOpTypeInfo TraceOp RefMatrix = RefMatrix

--- Step 1: Adding metadata/type data
binOpMeta :: BinOp -> Ref -> MetaInfo -> MetaInfo -> MetaInfo
binOpMeta op d l r = MetaInfo {
    ref=d,
    invar=(isInvarBin (invar l) (invar r)),
    typeInfo=(fst $ getBinOpTypeInfo op (typeInfo l) (typeInfo r))
    }
    where
        isInvarBin IsntInvariant IsntInvariant = IsntInvariant
        isInvarBin IsInvariant IsntInvariant = IsntInvariant
        isInvarBin IsntInvariant IsInvariant = IsntInvariant
        isInvarBin IsInvariant IsInvariant = IsInvariant

unOpMeta :: UnOp -> Ref -> MetaInfo -> MetaInfo
unOpMeta op d arg = MetaInfo {ref=d, invar=(invar arg), typeInfo=(getUnOpTypeInfo op (typeInfo arg))}

metaInfoOn :: MetaDB -> TAC -> MetaDB
metaInfoOn mdb (TAC d (BinOpTac op l r)) = M.insert d (binOpMeta op d (mdb M.! l) (mdb M.! r)) mdb
metaInfoOn mdb (TAC d (UnOpTac op arg)) = M.insert d (unOpMeta op d (mdb M.! arg)) mdb

addMetaInfo :: [TAC] -> MetaDB -> MetaDB
addMetaInfo tac mdb = foldl' metaInfoOn mdb tac

-- Step 2: Cannonicalization. Flip the args so equivilant expressions hash to the same value
cannonicalize :: MetaDB -> TAC -> TAC
cannonicalize _ (TAC d (UnOpTac op arg)) = (TAC d (UnOpTac op arg))

cannonicalize mdb qq@(TAC d (BinOpTac op r l)) = unsafePerformIO $ do
    return (TAC d (BinOpTac op rr ll))
    where
        lookup a = typeInfo (mdb M.! a)
        abelianness = snd $ getBinOpTypeInfo op (lookup r) (lookup l)
        shouldSwap = abelianness==IsAbelian && (r > l)
        (rr,ll) = if shouldSwap then (l,r) else (r,l)

-- Step 3: Subexpression elimination.
type SubExpElimDB = M.Map TACOp Ref
type RewriteDB = M.Map Ref Ref

data Shit = Shit

performArgumentSubstitution x rwDB = mat x
    where
        gs x = M.findWithDefault x x rwDB
        mat (BinOpTac op a b) = BinOpTac op (gs a) (gs b)
        mat (UnOpTac op arg) = UnOpTac op (gs arg)

subExpAnalyze :: ([TAC], SubExpElimDB, RewriteDB) -> TAC -> ([TAC], SubExpElimDB, RewriteDB)
subExpAnalyze (tacSoFar,seeDB,rwDB) thisTac@(TAC dest op) = (tacSoFar ++ (fst ns'), newSEEDB, snd ns')
    where
        thisTac' = (TAC dest (performArgumentSubstitution op rwDB))
        newSEEDB = M.insert op dest seeDB
        ns (Nothing) = ([thisTac'], rwDB) -- No common subexpression
        -- Found a similar subexp. Kill the current line and throw in a rewrite
        ns (Just equivRef) = ([], M.insert dest equivRef rwDB)
        ns' = ns $ M.lookup op seeDB

subExpElim tac = foldl' subExpAnalyze ([],(M.empty),(M.empty)) tac

runDagger :: TACM () -> IO ()
runDagger x = do
    m <- runStateT x emptyTACDB
    let db = snd m
    let populatedMeta = addMetaInfo (tacAssembly db) (meta db)
    let cannonicalizedAsm = map (cannonicalize populatedMeta) (tacAssembly db)
    --putStrLn $ show populatedMeta
    let (seeTac,_,eliminated) = subExpElim cannonicalizedAsm
    putStrLn "~~~~ UNOPTIMIZED TAC ~~~~"
    putStrLn $ dumpTACListing (tacAssembly db)
    putStrLn "~~~~~ OPTIMIZED TAC ~~~~~"
    putStrLn $ dumpTACListing (seeTac)
    return ()
