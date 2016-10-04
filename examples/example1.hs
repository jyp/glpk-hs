import Prelude hiding (Num(..))

import Algebra.Classes
import Control.Monad.LPMonad
import Data.LinearProgram.Common
import Data.LinearProgram
import Data.LinearProgram.GLPK
import qualified Data.Map as M
import Data.LinearProgram.LinExpr

objFun :: LinFunc String Int
objFun = linCombination [(10, "x1"), (6, "x2"), (4, "x3")]

n *& v = linCombination [(n,v)]

lp :: LP String Int
lp = execLPM $ do
  setDirection Max
  setObjective objFun
  leqTo (add $ map (1 *&) ["x1", "x2", "x3"]) 100
  leqTo (10 *& "x1" + 4 *& "x2" + 5 *& "x3") 600
  leqTo (linCombination [(2, "x1"), (2, "x2"), (6, "x3")]) 300
  varGeq "x1" 0
  varBds "x2" 0 50
  varGeq "x3" 0
  setVarKind "x1" IntVar
  setVarKind "x2" ContVar

main = print =<< glpSolveVars mipDefaults lp
