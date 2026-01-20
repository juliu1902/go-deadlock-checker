module Duality where
import Datastructure
import Equivalence
import qualified Data.Map as Map


testDuality :: Context -> Context -> [Expr] -> [VarName] -> [VarName] -> Statement -> Statement -> IO Bool
testDuality ctxt1 ctxt2 assumptions closeds locals st1 st2 = do -- simplification and canonicalization
    case (simplification st1, simplification st2) of
        (Skip, Skip) -> return True -- ATOM-DUAL-SKIP
        (Make oid _ s1, s2) -> do
            if oid `notElem` (locals ++ closeds) then 
                -- MAKE-L
                testDuality ctxt1 ctxt2 assumptions closeds (locals ++ [oid]) s1 s2
                -- MAKE-RENAME-L
                else let fresh = freshLocal (length locals)
                      in testDuality ctxt1 ctxt2 assumptions closeds (locals ++ [fresh]) (renameChan oid fresh s1) s2
        (s1, Make oid _ s2) -> do
            if oid `notElem` (locals ++ closeds) then 
                -- MAKE-R
                testDuality ctxt1 ctxt2 assumptions closeds (locals ++ [oid]) s1 s2
                -- MAKE-RENAME-R
                else let fresh = freshLocal (length locals)
                      in testDuality ctxt1 ctxt2 assumptions closeds (locals ++ [fresh]) s1 (renameChan oid fresh s2)
        -- DECLARE
        (Sequence (Declare v t) s1, s2) -> do
            case t of
                TBool -> testDuality (Map.insert v (TBool, AUnknown) ctxt1) ctxt2 assumptions closeds locals s1 s2
                TInt -> testDuality (Map.insert v (TInt, AUnknown) ctxt1) ctxt2 assumptions closeds locals s1 s2
                _ -> testDuality ctxt1 ctxt2 assumptions closeds locals s1 s2
        -- DECLARE SYMMETRIC
        (s1, Sequence (Declare v t) s2) -> do
            case t of
                TBool -> testDuality ctxt1 (Map.insert v (TBool, AUnknown) ctxt2) assumptions closeds locals s1 s2
                TInt -> testDuality ctxt1 (Map.insert v (TInt, AUnknown) ctxt2) assumptions closeds locals s1 s2
                _ -> testDuality ctxt1 ctxt2 assumptions closeds locals s1 s2
        (Sequence (Send x) s1, s2) -- MAKE-SEND-L
            | x `elem` locals -> testDuality ctxt1 ctxt2 assumptions closeds locals s1 s2
        (s1, Sequence (Send x) s2) -- MAKE-SEND-R
            | x `elem` locals -> testDuality ctxt1 ctxt2 assumptions closeds locals s1 s2
        (Sequence (Receive x) s1, s2) -- MAKE-RECV-L
            | x `elem` locals -> testDuality ctxt1 ctxt2 assumptions closeds locals s1 s2
        (s1, Sequence (Receive x) s2) -- MAKE-RECV-R
            | x `elem` locals -> testDuality ctxt1 ctxt2 assumptions closeds locals s1 s2
        (Sequence (End x) s1, s2) -- MAKE-CLOSE-L
            | x `elem` locals -> testDuality ctxt1 ctxt2 assumptions closeds locals s1 s2
        (s1, Sequence (End x) s2) -- MAKE-CLOSE-R
            | x `elem` locals -> testDuality ctxt1 ctxt2 assumptions closeds locals s1 s2
        (Sequence (Send x) s1, Sequence (Receive y) s2) -> do -- SEQUENCE-DUAL-SEND
            res <- testDuality ctxt1 ctxt2 assumptions closeds locals s1 s2
            return (x == y && res)
        (Sequence (Receive x) s1, Sequence (Send y) s2) -> do -- SEQUENCE-DUAL-RECV
            res <- testDuality ctxt1 ctxt2 assumptions closeds locals s1 s2
            return (x == y && res)
        (Sequence (End x) s1, s2) -> do -- L-CLOSE
            res <- testDuality ctxt1 ctxt2 assumptions (closeds ++ [x]) locals s1 s2
            return (x `notElem` closeds && res)
        (s1, Sequence (End x) s2) -> do -- R-CLOSE
            res <- testDuality ctxt1 ctxt2 assumptions (closeds ++ [x]) locals s1 s2
            return (x `notElem`closeds && res)
        (s, (Sequence (If e s1 s2) s0)) -> do -- COND-RIGHT-SPLIT
            unsat1 <- assumptionsUnsat ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)])
            r1 <- testDuality ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) closeds locals s (Sequence s1 s0)
            unsat2 <- assumptionsUnsat ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)])
            r2 <- testDuality ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) closeds locals s (Sequence s2 s0)
            case (unsat1, unsat2) of
                (True, True) -> return True
                (True, False) -> return r2 
                (False, True) -> return r1 
                (False, False) -> return (r1&&r2)
        (Sequence (If e s1 s2) s0, s) -> do -- COND-LEFT-SPLIT
            unsat1 <- assumptionsUnsat ctxt1 (assumptions ++ [EBinOp Eq e (EBool True)])
            r1 <- testDuality ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) closeds locals s (Sequence s1 s0)
            unsat2 <- assumptionsUnsat ctxt1 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)])
            r2 <- testDuality ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) closeds locals s (Sequence s2 s0)
            case (unsat1, unsat2) of
                (True, True) -> return True
                (True, False) -> return r2
                (False, True) -> return r1
                (False, False) -> return (r1&&r2)
        _ -> return False

-- for MAKE-RENAME
renameChan :: VarName -> VarName -> Statement -> Statement
renameChan old new st = case st of
  Send x       -> Send (if x == old then new else x)
  Receive x   -> Receive (if x == old then new else x)
  End x       -> End (if x == old then new else x)
  Make v t s   -> Make (if v == old then new else v) t (renameChan old new s)
  Sequence s1 s2 -> Sequence (renameChan old new s1) (renameChan old new s2)
  If e a b     -> If e (renameChan old new a) (renameChan old new b)
  x        -> x

freshLocal :: Int -> VarName
freshLocal n = VarName ("$loc" ++ show n)

getChanID :: Context -> VarName -> Maybe ChannelID
getChanID ctxt v =
  case Map.lookup v ctxt of
    Just (_, AChan cid) -> Just cid
    _                   -> Nothing

-- UNSAT(C)?
assumptionsUnsat :: Context -> [Expr] -> IO Bool
assumptionsUnsat ctxt assumptions = do
  let conj =
        foldr (\x y -> EBinOp And x y) (EBool True) assumptions
  res <- checkSatForEquiv conj ctxt
  return (isUnsat res)

assumptionsUnsatDual :: Context -> Context -> [Expr] -> IO Bool
assumptionsUnsatDual ctxt1 ctxt2 assumptions = do
  let conj =
        foldr (\x y -> EBinOp And x y) (EBool True) assumptions
  res1 <- checkSatForEquiv conj ctxt1
  res2 <- checkSatForEquiv conj ctxt2
  return (isUnsat res1 || isUnsat res2)

