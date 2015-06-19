-- | Contains sufficient tools to represent linear programming problems in Haskell.  In the future, if linkings to other
-- linear programming libraries are made, this will be common to them all.
module Data.LinearProgram.Common (
        module Data.LinearProgram.Spec,
        module Numeric.Additive.Group,
        module Numeric.Ring.Class,
        module Numeric.Field.Class,
        module Numeric.Module.Class,
        module Data.LinearProgram.Types) where

import Data.LinearProgram.Spec
import Numeric.Additive.Group
import Numeric.Ring.Class
import Numeric.Field.Class
import Numeric.Module.Class
import Data.LinearProgram.Types

import Data.Map
import GHC.Exts (build)

{-# RULES
        "assocs" assocs = \ m -> build (\ c n -> foldWithKey (curry c) n m);
        "elems" elems = \ m -> build (\ c n -> foldWithKey (const c) n m);
        "keys" keys = \ m -> build (\ c n -> foldWithKey (\ k _ -> c k) n m);
        #-}
