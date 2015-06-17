
import Data.LinearProgram.LPMonad
import Data.LinearProgram
import Data.LinearProgram.GLPK

objFun :: LinFunc String Int
objFun = linCombination [(10, "x1"), (6, "x2"), (4, "x3")]

lp :: LP String Int
lp = execLPM $ do	setDirection Max
			setObjective objFun
			leqTo (varSum ["x1", "x2", "x3"]) 100
			leqTo (10 *^ var "x1" ^+^ 4 *& "x2" ^+^ 5 *^ var "x3") 600
			leqTo (linCombination [(2, "x1"), (2, "x2"), (6, "x3")]) 300
			varGeq "x1" 0
			varBds "x2" 0 50
			varGeq "x3" 0
			setVarKind "x1" IntVar
			setVarKind "x2" ContVar

main = print =<< glpSolveVars mipDefaults lp