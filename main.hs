-- | Main entry point to the application.
module Main where

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import System.IO.Unsafe

import Types
import DSL
import Transform

test = runDagger $ do
    a   <- getRef IsntInvariant RefMatrix "A"
    b   <- getRef IsntInvariant RefMatrix "B"

    x   <- getRef IsntInvariant RefScalar "x"
    y   <- getRef IsntInvariant RefScalar "y"

    ast2tac $ (a*b + b*a)
    ast2tac $ (y*x + x*y)*a

    return ()

main = do
    test
