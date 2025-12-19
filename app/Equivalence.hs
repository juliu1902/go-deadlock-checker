module Equivalence where

import Datastructure
import qualified Data.Map            as Map (Map, delete, deleteMin, empty,
                                             fromList, insert, lookup,
                                             lookupMin, map, mapWithKey, toList,
                                             union)
import           Data.SBV            (SBV, SBool, SInteger, SMTResult (..),
                                      SatResult (..), Symbolic, literal, runSMT,
                                      sBool, sDiv, sInteger, sMod, sNot, sTrue,
                                      sat, (.&&), (.<), (.<=), (.==), (.>),
                                      (.>=), (.||), free, ite)
--data Expr
--  = EVar VarName
--  | EBool Bool
--  | EInt Integer
--  | EFloat Double
--  | EBinOp BinOp Expr Expr
--  | ENot Expr
--  deriving (Eq)
--testEquivalence' :: Context -> Context -> [Expr] -> [Expr] -> Statement -> Statement -> IO Bool
--testEquivalence' ctxt1 ctxt2 assumptions1 assumptions2 s1 s2 = do
--  case (s1, s2) of
--    (Send x, Send y) -> return (x == y) -- atom-send
--    (Receive x, Receive y) -> return (x == y) -- atom-recv
--    (End x, End y) -> return (x == y) -- atom-end
--  -- SEQ-SEND
--    (Sequence (Send x) s1, Sequence (Send y) s2) -> do
--      ctxtAfterSend1 <- getContext (Send x) ctxt1
--      ctxtAfterSend2 <- getContext (Send y) ctxt2
--      res <- testEquivalence' ctxtAfterSend1 ctxtAfterSend2 assumptions1 assumptions2 s1 s2
--      return (x == y && res)
--  -- SEQ-RECV
--    (Sequence (Receive x) s1, Sequence (Receive y) s2) -> do
--      ctxtAfterRecv1 <- getContext (Receive x) ctxt1
--      ctxtAfterRecv2 <- getContext (Receive y) ctxt2
--      res <- testEquivalence' ctxtAfterRecv1 ctxtAfterRecv2 assumptions1 assumptions2 s1 s2
--      return (x == y && res)
--  -- SEQ-END
--    (Sequence (End x) s1, Sequence (End y) s2) -> do
--      ctxtAfterEnd1 <- getContext (End x) ctxt1
--      ctxtAfterEnd2 <- getContext (End y) ctxt2
--      res <- testEquivalence' ctxtAfterEnd1 ctxtAfterEnd2 assumptions1 assumptions2 s1 s2
--      return (x == y && res)
--  -- CONDS
--    (Sequence (If e s1 s2) s0, s) -> do
--      resEntails <- entails ctxt1 assumptions1 e
--      case resEntails of
--      --COND-LEFT-THEN
--        True -> do
--          ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
--          testEquivalence' ctxtAfterIf1 ctxt2 assumptions1 assumptions2 (Sequence s1 s0) s
--        False -> do
--          resEntails2 <- entails ctxt1 assumptions1 (ENot e)
--          case resEntails2 of
--          -- COND-LEFT-ELSE
--            True -> do
--              ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
--              testEquivalence' ctxtAfterIf1 ctxt2 assumptions1 assumptions2 (Sequence s2 s0) s
--          -- COND-LEFT-SPLIT
--            False -> do
--              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions1) ctxt1
--              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions1) ctxt1
--              let thenOK = not (isUnsat satE)
--                  elseOK = not (isUnsat satNotE)
--              case (thenOK, elseOK) of
--                (True, True) -> do 
--                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
--                  r1 <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions1 ++ [EBinOp Eq e (EBool True)]) assumptions2 (Sequence s1 s0) s
--                  r2 <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions1 ++ [EBinOp Eq (ENot e) (EBool True)]) assumptions2 (Sequence s2 s0) s
--                  return (r1 && r2)
--                (False, True) -> do
--                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
--                  res <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions1 ++ [EBinOp Eq (ENot e) (EBool True)]) assumptions2 (Sequence s2 s0) s
--                  return res
--                (True, False) -> do
--                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
--                  res <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions1 ++ [EBinOp Eq e (EBool True)]) assumptions2 (Sequence s1 s0) s
--                  return $ res
--                _ -> do
--                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1 
--                  testEquivalence' ctxtAfterIf1 ctxt2 assumptions1 assumptions2 s0 s
--    (s, Sequence (If e s1 s2) s0) -> do
--      resEntails <- entails ctxt2 assumptions2 e
--      case resEntails of
--      --COND-RIGHT-THEN
--        True -> do
--          ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--          testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 assumptions2 s (Sequence s1 s0)
--        False -> do
--          resEntails2 <- entails ctxt2 assumptions2 (ENot e)
--          case resEntails2 of
--          -- COND-RIGHT-ELSE
--            True -> do
--              ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--              testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 assumptions2 s (Sequence s2 s0)
--          -- COND-RIGHT-SPLIT
--            False -> do
--              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions2) ctxt2
--              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions2) ctxt2
--              let thenOK = not (isUnsat satE)
--                  elseOK = not (isUnsat satNotE)
--              case (thenOK, elseOK) of
--                (True, True) -> do 
--                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--                  r1 <- testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 (assumptions2 ++ [EBinOp Eq e (EBool True)]) s (Sequence s1 s0)
--                  r2 <- testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 (assumptions2 ++ [EBinOp Eq (ENot e) (EBool True)]) s (Sequence s2 s0)
--                  return (r1 && r2)
--                (False, True) -> do
--                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--                  res <- testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 (assumptions2 ++ [EBinOp Eq (ENot e) (EBool True)]) s (Sequence s2 s0)
--                  return res
--                (True, False) -> do
--                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--                  res <- testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 (assumptions2 ++ [EBinOp Eq e (EBool True)]) s (Sequence s1 s0)
--                  return $ res
--                _ -> do
--                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--                  res <- testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 assumptions2 s s0
--                  return $ res
--    (If e s1 s2, s) -> do
--      resEntails <- entails ctxt1 assumptions1 e
--      case resEntails of
--      --COND-LEFT-THEN
--        True -> do
--          ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1 
--          testEquivalence' ctxtAfterIf1 ctxt2 assumptions1 assumptions2 s1 s
--        False -> do
--          resEntails2 <- entails ctxt1 assumptions1 (ENot e)
--          case resEntails2 of
--          -- COND-LEFT-ELSE
--            True -> do
--              ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
--              testEquivalence' ctxtAfterIf1 ctxt2 assumptions1 assumptions2 s2 s
--          -- COND-LEFT-SPLIT
--            False -> do
--              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions1) ctxt1
--              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions1) ctxt1
--              let thenOK = not (isUnsat satE)
--                  elseOK = not (isUnsat satNotE)
--              case (thenOK, elseOK) of
--                (True, True) -> do 
--                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
--                  r1 <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions1 ++ [EBinOp Eq e (EBool True)]) assumptions2 s1 s
--                  r2 <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions1 ++ [EBinOp Eq (ENot e) (EBool True)]) assumptions2 s2 s
--                  return (r1 && r2)
--                (False, True) -> do
--                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
--                  res <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions1 ++ [EBinOp Eq (ENot e) (EBool True)]) assumptions2 s2 s
--                  return res
--                (True, False) -> do
--                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
--                  res <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions1 ++ [EBinOp Eq e (EBool True)]) assumptions2 s1 s
--                  return $ res
--                _ -> return $ False
--    (s, If e s1 s2) -> do
--      resEntails <- entails ctxt2 assumptions2 e
--      case resEntails of
--      --COND-LEFT-THEN
--        True -> do
--          ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--          testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 assumptions2 s s1 
--        False -> do
--          resEntails2 <- entails ctxt2 assumptions2 (ENot e)
--          case resEntails2 of
--          -- COND-LEFT-ELSE
--            True -> do
--              ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--              testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 assumptions2 s s2 
--          -- COND-LEFT-SPLIT
--            False -> do
--              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions2) ctxt2
--              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions2) ctxt2
--              let thenOK = not (isUnsat satE)
--                  elseOK = not (isUnsat satNotE)
--              case (thenOK, elseOK) of
--                (True, True) -> do 
--                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--                  r1 <- testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 (assumptions2 ++ [EBinOp Eq e (EBool True)]) s s1 
--                  r2 <- testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 (assumptions2 ++ [EBinOp Eq (ENot e) (EBool True)]) s s2
--                  return (r1 && r2)
--                (False, True) -> do
--                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--                  res <- testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 (assumptions2 ++ [EBinOp Eq (ENot e) (EBool True)]) s s2 
--                  return res
--                (True, False) -> do
--                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
--                  res <- testEquivalence' ctxt1 ctxtAfterIf2 assumptions1 (assumptions2 ++ [EBinOp Eq e (EBool True)]) s s1 
--                  return $ res
--                _ -> return False
--    (Skip, Skip) -> return True
--    (Sequence s1 Skip, s) -> do
--      ctxtAfter1 <- getContext s1 ctxt1
--      testEquivalence' ctxtAfter1 ctxt2 assumptions1 assumptions2 s1 s
--    (Sequence Skip s1, s) -> do
--      ctxtAfter1 <- getContext s1 ctxt1
--      testEquivalence' ctxtAfter1 ctxt2 assumptions1 assumptions2 s1 s
--    (s, Sequence s1 Skip) -> do
--      ctxtAfter2 <- getContext s1 ctxt2
--      testEquivalence' ctxt1 ctxtAfter2 assumptions1 assumptions2 s s1
--    (s, Sequence Skip s1) -> do
--      ctxtAfter2 <- getContext s1 ctxt2
--      testEquivalence' ctxt1 ctxtAfter2 assumptions1 assumptions2 s s1
--    (Sequence s1 s2, Sequence s3 s4) -> do 
--      ctxtAfter1 <- getContext s1 ctxt1
--      ctxtAfter2 <- getContext s3 ctxt2
--      res1 <- testEquivalence' ctxtAfter1 ctxtAfter2 assumptions1 assumptions2 s2 s4
--      res2 <- testEquivalence' ctxt1 ctxt2 assumptions1 assumptions2 s1 s2
--      return $ res1 && res2 
--    _ -> return $ False

testEquivalence' :: Context -> Context -> [Expr] -> Statement -> Statement -> IO Bool
testEquivalence' ctxt1 ctxt2 assumptions s1 s2 = do
  case (s1, s2) of
    (Skip, Skip) -> return True
    (Send x, Send y) -> return (x == y) -- atom-send
    (Receive x, Receive y) -> return (x == y) -- atom-recv
    (End x, End y) -> return (x == y) -- atom-end
  -- SEQ-SEND
    (Sequence (Send x) s1, Sequence (Send y) s2) -> do
      ctxtAfterSend1 <- getContext (Send x) ctxt1
      ctxtAfterSend2 <- getContext (Send y) ctxt2
      res <- testEquivalence' ctxtAfterSend1 ctxtAfterSend2 assumptions s1 s2
      return (x == y && res)
  -- SEQ-RECV
    (Sequence (Receive x) s1, Sequence (Receive y) s2) -> do
      ctxtAfterRecv1 <- getContext (Receive x) ctxt1
      ctxtAfterRecv2 <- getContext (Receive y) ctxt2
      res <- testEquivalence' ctxtAfterRecv1 ctxtAfterRecv2 assumptions s1 s2
      return (x == y && res)
  -- SEQ-END
    (Sequence (End x) s1, Sequence (End y) s2) -> do
      ctxtAfterEnd1 <- getContext (End x) ctxt1
      ctxtAfterEnd2 <- getContext (End y) ctxt2
      res <- testEquivalence' ctxtAfterEnd1 ctxtAfterEnd2 assumptions s1 s2
      return (x == y && res)
  -- DECLARE / MAKE
    (Sequence (Declare v t) s1, s2) -> do
      ctxt1' <- getContext (Declare v t) ctxt1
      testEquivalence' ctxt1' ctxt2 assumptions s1 s2

    (Sequence (Make v ct) s1, s2) -> do
      ctxt1' <- getContext (Make v ct) ctxt1
      testEquivalence' ctxt1' ctxt2 assumptions s1 s2

  -- DECLARE / MAKE symmetric
    (s1, Sequence (Declare v t) s2) -> do
      ctxt2' <- getContext (Declare v t) ctxt2
      testEquivalence' ctxt1 ctxt2' assumptions s1 s2

    (s1, Sequence (Make v ct) s2) -> do
      ctxt2' <- getContext (Make v ct) ctxt2
      testEquivalence' ctxt1 ctxt2' assumptions s1 s2

  -- CONDS
    (Sequence (If e s1 s2) s0, s) -> do
      resEntails <- entails ctxt1 assumptions e
      case resEntails of
      --COND-LEFT-THEN
        True -> do
          ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
          testEquivalence' ctxtAfterIf1 ctxt2 assumptions (Sequence s1 s0) s
        False -> do
          resEntails2 <- entails ctxt1 assumptions (ENot e)
          case resEntails2 of
          -- COND-LEFT-ELSE
            True -> do
              ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
              testEquivalence' ctxtAfterIf1 ctxt2 assumptions (Sequence s2 s0) s
          -- COND-LEFT-SPLIT
            False -> do
              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions) ctxt1
              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions) ctxt1
              let thenOK = not (isUnsat satE)
                  elseOK = not (isUnsat satNotE)
              case (thenOK, elseOK) of
                (True, True) -> do 
                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
                  r1 <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) (Sequence s1 s0) s
                  r2 <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) (Sequence s2 s0) s
                  return (r1 && r2)
                (False, True) -> do
                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
                  res <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) (Sequence s2 s0) s
                  return res
                (True, False) -> do
                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
                  res <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) (Sequence s1 s0) s
                  return $ res
                _ -> do
                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1 
                  testEquivalence' ctxtAfterIf1 ctxt2 assumptions s0 s
    (s, Sequence (If e s1 s2) s0) -> do
      resEntails <- entails ctxt2 assumptions e
      case resEntails of
      --COND-RIGHT-THEN
        True -> do
          ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
          testEquivalence' ctxt1 ctxtAfterIf2 assumptions s (Sequence s1 s0)
        False -> do
          resEntails2 <- entails ctxt2 assumptions (ENot e)
          case resEntails2 of
          -- COND-RIGHT-ELSE
            True -> do
              ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
              testEquivalence' ctxt1 ctxtAfterIf2 assumptions s (Sequence s2 s0)
          -- COND-RIGHT-SPLIT
            False -> do
              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions) ctxt2
              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions) ctxt2
              let thenOK = not (isUnsat satE)
                  elseOK = not (isUnsat satNotE)
              case (thenOK, elseOK) of
                (True, True) -> do 
                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
                  r1 <- testEquivalence' ctxt1 ctxtAfterIf2 (assumptions ++ [EBinOp Eq e (EBool True)]) s (Sequence s1 s0)
                  r2 <- testEquivalence' ctxt1 ctxtAfterIf2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s (Sequence s2 s0)
                  return (r1 && r2)
                (False, True) -> do
                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
                  res <- testEquivalence' ctxt1 ctxtAfterIf2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s (Sequence s2 s0)
                  return res
                (True, False) -> do
                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
                  res <- testEquivalence' ctxt1 ctxtAfterIf2 (assumptions ++ [EBinOp Eq e (EBool True)]) s (Sequence s1 s0)
                  return $ res
                _ -> do
                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
                  res <- testEquivalence' ctxt1 ctxtAfterIf2  assumptions s s0
                  return $ res
    (If e s1 s2, s) -> do
      resEntails <- entails ctxt1 assumptions e
      case resEntails of
      --COND-LEFT-THEN
        True -> do
          ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1 
          testEquivalence' ctxtAfterIf1 ctxt2 assumptions s1 s
        False -> do
          resEntails2 <- entails ctxt1 assumptions (ENot e)
          case resEntails2 of
          -- COND-LEFT-ELSE
            True -> do
              ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
              testEquivalence' ctxtAfterIf1 ctxt2 assumptions s2 s
          -- COND-LEFT-SPLIT
            False -> do
              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions) ctxt1
              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions) ctxt1
              let thenOK = not (isUnsat satE)
                  elseOK = not (isUnsat satNotE)
              case (thenOK, elseOK) of
                (True, True) -> do 
                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
                  r1 <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) s1 s
                  r2 <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s2 s
                  return (r1 && r2)
                (False, True) -> do
                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
                  res <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s2 s
                  return res
                (True, False) -> do
                  ctxtAfterIf1 <- getContext (If e s1 s2) ctxt1
                  res <- testEquivalence' ctxtAfterIf1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) s1 s
                  return $ res
                _ -> return $ False
    (s, If e s1 s2) -> do
      resEntails <- entails ctxt2 assumptions e
      case resEntails of
      --COND-LEFT-THEN
        True -> do
          ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
          testEquivalence' ctxt1 ctxtAfterIf2 assumptions s s1 
        False -> do
          resEntails2 <- entails ctxt2 assumptions (ENot e)
          case resEntails2 of
          -- COND-LEFT-ELSE
            True -> do
              ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
              testEquivalence' ctxt1 ctxtAfterIf2 assumptions s s2 
          -- COND-LEFT-SPLIT
            False -> do
              satE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) e assumptions) ctxt2
              satNotE <- checkSatForEquiv (foldr (\x y -> EBinOp And x y) (ENot e) assumptions) ctxt2
              let thenOK = not (isUnsat satE)
                  elseOK = not (isUnsat satNotE)
              case (thenOK, elseOK) of
                (True, True) -> do 
                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
                  r1 <- testEquivalence' ctxt1 ctxtAfterIf2 (assumptions ++ [EBinOp Eq e (EBool True)]) s s1 
                  r2 <- testEquivalence' ctxt1 ctxtAfterIf2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s s2
                  return (r1 && r2)
                (False, True) -> do
                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
                  res <- testEquivalence' ctxt1 ctxtAfterIf2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) s s2 
                  return res
                (True, False) -> do
                  ctxtAfterIf2 <- getContext (If e s1 s2) ctxt2
                  res <- testEquivalence' ctxt1 ctxtAfterIf2 (assumptions ++ [EBinOp Eq e (EBool True)]) s s1 
                  return $ res
                _ -> return False
    (Sequence s1 Skip, s) -> do
      ctxtAfter1 <- getContext s1 ctxt1
      testEquivalence' ctxtAfter1 ctxt2 assumptions s1 s
    (Sequence Skip s1, s) -> do
      ctxtAfter1 <- getContext s1 ctxt1
      testEquivalence' ctxtAfter1 ctxt2 assumptions s1 s
    (s, Sequence s1 Skip) -> do
      ctxtAfter2 <- getContext s1 ctxt2
      testEquivalence' ctxt1 ctxtAfter2 assumptions s s1
    (s, Sequence Skip s1) -> do
      ctxtAfter2 <- getContext s1 ctxt2
      testEquivalence' ctxt1 ctxtAfter2 assumptions s s1
    (Sequence s1 s2, Sequence s3 s4) -> do 
      ctxtAfter1 <- getContext s1 ctxt1
      ctxtAfter2 <- getContext s3 ctxt2
      res1 <- testEquivalence' ctxtAfter1 ctxtAfter2 assumptions s2 s4
      res2 <- testEquivalence' ctxt1 ctxt2 assumptions s1 s3
      return $ res1 && res2 
    _ -> return $ False
checkSatForEquiv :: Expr -> Context -> IO SatResult
checkSatForEquiv expr ctxt =
  sat $ do
    env <- mkEnv expr ctxt
    v   <- expressionToSymbolicForEquiv env expr ctxt
    case v of
      SBVBool b -> return b
      _         -> return sTrue

getContext :: Statement -> Context -> IO Context
getContext st ctxt = case st of
  Declare v t -> return (Map.insert v (t, AUnknown) ctxt)
  Make v chant -> let freshId = ChannelID ("oid" ++ show (length (Map.toList ctxt)))
                   in return (updateOneAV v (AChan freshId) ctxt)
  Sequence s1 s2 -> do
    ctxt <- getContext s1 ctxt
    getContext s2 ctxt
  If e s1 s2 -> do
    ctxt <- getContext s1 ctxt
    ctxt <- getContext s2 ctxt
    let e' = evalExpr e ctxt
        ctxtMerged = (mergeIfContexts e' ctxt ctxt)
     in return ctxtMerged
  Assign var (EVar x) ->
    case lookupAV x ctxt of
      ATerm e -> return (updateOneAV var (ATerm e) ctxt) 
      AIf e av1 av2 -> return (updateOneAV var (AIf e av1 av2) ctxt)
      _ -> return (updateOneAV var (ATerm (EVar x)) ctxt)
  Assign var (EBinOp op e1 e2) ->
    let e1' = evalExpr e1 ctxt
        e2' = evalExpr e2 ctxt
     in return (updateOneAV var (ATerm (EBinOp op e1' e2')) ctxt)
  Assign var (ENot expr) ->
    let expr' = evalExpr (ENot expr) ctxt 
     in return (updateOneAV var (ATerm expr') ctxt) 
  Assign var expr -> 
    let expr' = evalExpr expr ctxt
     in return (updateOneAV var (ATerm expr') ctxt)
  _ -> return ctxt


-- Is an expression always true under the given assumptions?
entails :: Context -> [Expr] -> Expr -> IO Bool
entails ctxt assumptions expr = do
  -- Prüfen ob (assumptions AND NOT expr) unerfüllbar ist
  let negatedExpr = ENot expr
      allAssumptions = assumptions ++ [negatedExpr]
      conjunctiveFormula =
        foldr (\x y -> EBinOp And x y) (EBool True) allAssumptions
  result <- checkSatForEquiv conjunctiveFormula ctxt
  return (isUnsat result)

expressionToSymbolicForEquiv :: SMTEnv -> Expr -> Context -> Symbolic SBVal
expressionToSymbolicForEquiv env expr ctxt =
  case expr of
    EBool b -> return (SBVBool $ literal b)
    EInt n -> return (SBVInt $ literal n)
    EVar x ->
      case Map.lookup x env of
        Just v  -> return v
        Nothing -> error ("SMTEnv missing var: " ++ show x)
    ENot e -> do
      val <- expressionToSymbolicForEquiv env e ctxt
      case val of
        SBVBool b -> return (SBVBool (sNot b))
        _         -> error "negating non-bool expression"
    EBinOp op e1 e2 -> do
      first <- expressionToSymbolicForEquiv env e1 ctxt
      second <- expressionToSymbolicForEquiv env e2 ctxt
      case op of
        And ->
          case (first, second) of
            (SBVBool b1, SBVBool b2) -> return (SBVBool (b1 .&& b2))
            _ ->
              error
                "at least one of the expressions of _ && _ might not be of type bool"
        Or ->
          case (first, second) of
            (SBVBool b1, SBVBool b2) -> return (SBVBool (b1 .|| b2))
            _ ->
              error
                "at least one of the expressions of _ || _ might not be of type bool"
        Lt ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (v1 .< v2))
            _ ->
              error
                "at least one of the expressions of _ || _ might not be of type int"
        Gt ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (v1 .> v2))
            _ ->
              error
                "at least one of the expressions of _ || _ might not be of type int"
        Le ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (v1 .<= v2))
            _ ->
              error
                "at least one of the expressions of _ || _ might not be of type int"
        Ge ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (v1 .>= v2))
            _ ->
              error
                "at least one of the expressions of _ || _ might not be of type int"
        Eq ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (v1 .== v2))
            (SBVBool b1, SBVBool b2) -> return (SBVBool (b1 .== b2))
            _ -> error "expressions of _ == _ not of same type"
        Neq ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVBool (sNot (v1 .== v2)))
            (SBVBool b1, SBVBool b2) -> return (SBVBool (sNot (b1 .== b2)))
            _ -> error "expressions of _ == _ not of same type"
        Add ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVInt (v1 + v2))
            _ ->
              error
                "at least one of the expressions of _ || _ might not be of type int"
        Sub ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVInt (v1 - v2))
            _ ->
              error
                "at least one of the expressions of _ || _ might not be of type int"
        Mul ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVInt (v1 * v2))
            _ ->
              error
                "at least one of the expressions of _ || _ might not be of type int"
        Mod ->
          case (first, second) of
            (SBVInt v1, SBVInt v2) -> return (SBVInt (sMod v1 v2))
            _ ->
              error
                "at least one of the expressions of _ || _ might not be of type int"
