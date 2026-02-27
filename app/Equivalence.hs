module Equivalence where

import Datastructure
import qualified Data.Map            as Map (Map, insert, lookup, toList, empty)
import           Data.SBV            (SatResult (..), Symbolic, literal,
                                      sMod, sNot, sTrue,
                                      sat, (.&&), (.<), (.<=), (.==), (.>),
                                      (.>=), (.||))
testEquivalence' :: Context -> Context -> [Expr] -> Statement -> Statement -> IO Bool
testEquivalence' ctxt1 ctxt2 assumptions st1 st2 = do -- simplification 
  case (simplification st1, simplification st2) of
    (Skip, Skip) -> return True -- atom-skip
  -- SEQ-SEND
    (Sequence (Send x) s1, Sequence (Send y) s2) -> do
      res <- testEquivalence' ctxt1 ctxt2 assumptions s1 s2
      return (x == y && res)
  -- SEQ-RECV
    (Sequence (Receive x) s1, Sequence (Receive y) s2) -> do
      res <- testEquivalence' ctxt1 ctxt2 assumptions s1 s2
      return (x == y && res)
  -- SEQ-END
    (Sequence (End x) s1, Sequence (End y) s2) -> do
      res <- testEquivalence' ctxt1 ctxt2 assumptions s1 s2
      return (x == y && res)
  -- DECLARE
    (Sequence (Declare v t) s1, s2) -> do  
      case t of
        TBool -> testEquivalence' (Map.insert v (TBool, AUnknown) ctxt1) ctxt2 assumptions s1 s2
        TInt -> testEquivalence' (Map.insert v (TInt, AUnknown) ctxt1) ctxt2 assumptions s1 s2
        _ -> testEquivalence' ctxt1 ctxt2 assumptions s1 s2
  -- DECLARE SYMMETRIC
    (s1, Sequence (Declare v t) s2) -> do  
      case t of
        TBool -> testEquivalence' ctxt1 (Map.insert v (TBool, AUnknown) ctxt2) assumptions s1 s2
        TInt -> testEquivalence' ctxt1 (Map.insert v (TInt, AUnknown) ctxt2) assumptions s1 s2
        _ -> testEquivalence' ctxt1 ctxt2 assumptions s1 s2  
  -- IGNORE MAKE
    (Make _ _ s1 , s2) -> testEquivalence' ctxt1 ctxt2 assumptions s1 s2 
    (s1, Make _ _ s2) -> testEquivalence' ctxt1 ctxt2 assumptions s1 s2 
  -- CONDS
    (Sequence (If e s1 s2) s0, s) -> do
      resEntails <- entails ctxt1 assumptions e
      case resEntails of
      --COND-LEFT-THEN
        True -> do
          testEquivalence' ctxt1 ctxt2 assumptions (Sequence s1 s0) s
        False -> do
          resEntails2 <- entails ctxt1 assumptions (ENot e)
          case resEntails2 of
          -- COND-LEFT-ELSE
            True -> do
              testEquivalence' ctxt1 ctxt2 assumptions (Sequence s2 s0) s
          -- COND-LEFT-SPLIT
            False -> do
              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions) ctxt1
              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions) ctxt1
              let thenOK = not (isUnsat satE)
                  elseOK = not (isUnsat satNotE)
              case (thenOK, elseOK) of
                (True, True) -> do
                  r1 <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) (Sequence s1 s0) s
                  r2 <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) (Sequence s2 s0) s
                  return (r1 && r2)
                (False, True) -> do
                  res <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) (Sequence s2 s0) s
                  return res
                (True, False) -> do
                  res <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) (Sequence s1 s0) s
                  return $ res
                _ -> do
                  testEquivalence' ctxt1 ctxt2 assumptions s0 s
    (s, Sequence (If e s1 s2) s0) -> do
      resEntails <- entails ctxt2 assumptions e
      case resEntails of
      --COND-RIGHT-THEN
        True -> do
          testEquivalence' ctxt1 ctxt2 assumptions s (Sequence s1 s0)
        False -> do
          resEntails2 <- entails ctxt2 assumptions (ENot e)
          case resEntails2 of
          -- COND-RIGHT-ELSE
            True -> do
              testEquivalence' ctxt1 ctxt2 assumptions s (Sequence s2 s0)
          -- COND-RIGHT-SPLIT
            False -> do
              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions) ctxt2
              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions) ctxt2
              let thenOK = not (isUnsat satE)
                  elseOK = not (isUnsat satNotE)
              case (thenOK, elseOK) of
                (True, True) -> do
                  r1 <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) s (Sequence s1 s0)
                  r2 <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s (Sequence s2 s0)
                  return (r1 && r2)
                (False, True) -> do
                  res <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s (Sequence s2 s0)
                  return res
                (True, False) -> do
                  res <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) s (Sequence s1 s0)
                  return $ res
                _ -> do
                  res <- testEquivalence' ctxt1 ctxt2  assumptions s s0
                  return $ res
    (If e s1 s2, s) -> do
      resEntails <- entails ctxt1 assumptions e
      case resEntails of
      --COND-LEFT-THEN
        True -> do
          testEquivalence' ctxt1 ctxt2 assumptions s1 s
        False -> do
          resEntails2 <- entails ctxt1 assumptions (ENot e)
          case resEntails2 of
          -- COND-LEFT-ELSE
            True -> do
              testEquivalence' ctxt1 ctxt2 assumptions s2 s
          -- COND-LEFT-SPLIT
            False -> do
              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions) ctxt1
              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions) ctxt1
              let thenOK = not (isUnsat satE)
                  elseOK = not (isUnsat satNotE)
              case (thenOK, elseOK) of
                (True, True) -> do
                  r1 <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) s1 s
                  r2 <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s2 s
                  return (r1 && r2)
                (False, True) -> do
                  res <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s2 s
                  return res
                (True, False) -> do
                  res <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) s1 s
                  return $ res
                _ -> return $ False
    (s, If e s1 s2) -> do
      resEntails <- entails ctxt2 assumptions e
      case resEntails of
      --COND-LEFT-THEN
        True -> do
          testEquivalence' ctxt1 ctxt2 assumptions s s1
        False -> do
          resEntails2 <- entails ctxt2 assumptions (ENot e)
          case resEntails2 of
          -- COND-LEFT-ELSE
            True -> do
              testEquivalence' ctxt1 ctxt2 assumptions s s2
          -- COND-LEFT-SPLIT
            False -> do
              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions) ctxt2
              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions) ctxt2
              let thenOK = not (isUnsat satE)
                  elseOK = not (isUnsat satNotE)
              case (thenOK, elseOK) of
                (True, True) -> do
                  r1 <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) s s1
                  r2 <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s s2
                  return (r1 && r2)
                (False, True) -> do
                  res <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s s2
                  return res
                (True, False) -> do
                  res <- testEquivalence' ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) s s1
                  return $ res
                _ -> return False
    (Sequence s1 Skip, s) -> do
      testEquivalence' ctxt1 ctxt2 assumptions s1 s
    (Sequence Skip s1, s) -> do
      testEquivalence' ctxt1 ctxt2 assumptions s1 s
    (s, Sequence s1 Skip) -> do
      testEquivalence' ctxt1 ctxt2 assumptions s s1
    (s, Sequence Skip s1) -> do
      testEquivalence' ctxt1 ctxt2 assumptions s s1
    (Sequence s1 s2, Sequence s3 s4) -> do
      res1 <- testEquivalence' ctxt1 ctxt2 assumptions s2 s4
      res2 <- testEquivalence' ctxt1 ctxt2 assumptions s1 s3
      return $ res1 && res2
    _ -> return False


checkSatForEquiv :: Expr -> Context -> IO Data.SBV.SatResult
checkSatForEquiv expr ctxt =
  Data.SBV.sat $ do
    env <- mkEnv expr ctxt
    v   <- expressionToSymbolicForEquiv env expr ctxt
    case v of
      SBVBool b -> return b
      _         -> return Data.SBV.sTrue

-- Is an expression always true under the given assumptions?
entails :: Context -> [Expr] -> Expr -> IO Bool
entails ctxt assumptions expr = do
  -- check if (assumptions AND NOT expr) is unsatisfiable
  let negatedExpr = ENot expr
      allAssumptions = assumptions ++ [negatedExpr]
      conjunctiveFormula =
        foldr (\x y -> EBinOp And x y) (EBool True) allAssumptions
  result <- checkSatForEquiv conjunctiveFormula ctxt
  return (isUnsat result)

expressionToSymbolicForEquiv :: SMTEnv -> Expr -> Context -> Data.SBV.Symbolic SBVal
expressionToSymbolicForEquiv env expr ctxt =
  case expr of
    EBool b -> return (SBVBool $ Data.SBV.literal b)
    EInt n -> return (SBVInt $ Data.SBV.literal n)
    EVar x ->
      case Map.lookup x env of
        Just v  -> return v
        Nothing -> error ("SMTEnv missing var: " ++ show x)
    ENot e -> do
      val <- expressionToSymbolicForEquiv env e ctxt
      case val of
        SBVBool b -> return (SBVBool (Data.SBV.sNot b))
        _         -> error "negating non-bool expression"
    EBinOp op e1 e2 -> do
      first <- expressionToSymbolicForEquiv env e1 ctxt
      second <- expressionToSymbolicForEquiv env e2 ctxt
      case op of
        And ->
          case (first, second) of
            (SBVBool b1, SBVBool b2) -> return (SBVBool (b1 Data.SBV..&& b2))
            _ ->
              error
                "at least one of the expressions of _ && _ might not be of type bool"
        Or ->
          case (first, second) of
            (SBVBool b1, SBVBool b2) -> return (SBVBool (b1 Data.SBV..|| b2))
            _ ->
              error
                "at least one of the expressions of _ || _ might not be of type bool"
        Lt ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (v1 Data.SBV..< v2))
            _ ->
              error
                "at least one of the expressions of _ < _ might not be of type int"
        Gt ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (v1 Data.SBV..> v2))
            _ ->
              error
                "at least one of the expressions of _ > _ might not be of type int"
        Le ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (v1 Data.SBV..<= v2))
            _ ->
              error
                "at least one of the expressions of _ <= _ might not be of type int"
        Ge ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (v1 Data.SBV..>= v2))
            _ ->
              error
                "at least one of the expressions of _ >= _ might not be of type int"
        Eq ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (v1 Data.SBV..== v2))
            (SBVBool b1, SBVBool b2) -> return (SBVBool (b1 Data.SBV..== b2))
            _ -> error "expressions of _ == _ not of same type"
        Neq ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (Data.SBV.sNot (v1 Data.SBV..== v2)))
            (SBVBool b1, SBVBool b2) -> return (SBVBool (Data.SBV.sNot (b1 Data.SBV..== b2)))
            _ -> error "expressions of _ == _ not of same type"
        Add ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVInt (v1 + v2))
            _ ->
              error
                "at least one of the expressions of _ + _ might not be of type int"
        Sub ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVInt (v1 - v2))
            _ ->
              error
                "at least one of the expressions of _ - _ might not be of type int"
        Mul ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVInt (v1 * v2))
            _ ->
              error
                "at least one of the expressions of _ * _ might not be of type int"
        Mod ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVInt (Data.SBV.sMod v1 v2))
            _ ->
              error
                "at least one of the expressions of _ % _ might not be of type int"
