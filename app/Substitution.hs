module Substitution where
import Datastructure
import Equivalence
import qualified Data.Map as Map


-- Helper function to substitute variables in a statement
substituteVars :: Map.Map VarName VarName -> Statement -> Statement
substituteVars subMap stmt = case stmt of
  Skip -> Skip
  Send var -> Send (substituteVar subMap var)
  Receive var -> Receive (substituteVar subMap var)
  End var -> End (substituteVar subMap var)
  Sequence s1 s2 -> Sequence (substituteVars subMap s1) (substituteVars subMap s2)
  If expr s1 s2 -> If (substituteVarsInExpr subMap expr) (substituteVars subMap s1) (substituteVars subMap s2)
  For fh s -> For (substituteVarsInForHeader subMap fh) (substituteVars subMap s)
  Assign var expr -> Assign (substituteVar subMap var) (substituteVarsInExpr subMap expr)
  Go s -> Go (substituteVars subMap s)
  Declare var vtype -> Declare (substituteVar subMap var) vtype
  Make var ctype s -> Make (substituteVar subMap var) ctype (substituteVars subMap s)
  Func var vdecs s -> Func (substituteVar subMap var) vdecs (substituteVars subMap s)
  GoCall var vars -> GoCall (substituteVar subMap var) (map (substituteVar subMap) vars)

-- Helper function to substitute a single variable
substituteVar :: Map.Map VarName VarName -> VarName -> VarName
substituteVar subMap var = case Map.lookup var subMap of
  Just newVar -> newVar
  Nothing -> var

-- Helper function to substitute variables in expressions
substituteVarsInExpr :: Map.Map VarName VarName -> Expr -> Expr
substituteVarsInExpr subMap expr = case expr of
  EVar var -> EVar (substituteVar subMap var)
  EInt i -> EInt i
  EFloat f -> EFloat f
  EBool b -> EBool b
  EBinOp op e1 e2 -> EBinOp op (substituteVarsInExpr subMap e1) (substituteVarsInExpr subMap e2)
  ENot e -> ENot (substituteVarsInExpr subMap e)

-- Helper function to substitute variables in for headers
substituteVarsInForHeader :: Map.Map VarName VarName -> ForHeader -> ForHeader
substituteVarsInForHeader subMap fh = case fh of
  ForHeaderRunning var int expr incdec -> 
    ForHeaderRunning (substituteVar subMap var) int (substituteVarsInExpr subMap expr) incdec
  ForHeaderRange var1 var2 -> 
    ForHeaderRange (substituteVar subMap var1) (substituteVar subMap var2)

