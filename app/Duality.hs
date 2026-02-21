{- HLINT ignore "Redundant if" -}
module Duality where
import Datastructure
import Equivalence
import Data.List as List
import qualified Data.Map as Map

data State = Closed | Fresh | External | Open deriving (Eq, Show)

type Z = Map.Map VarName State

-- DUAL-SEND-RECV and EXT-SEND can both be right at the same time!!! TODO
testDuality :: Context -> Context -> [Expr] -> Z -> Statement -> Statement -> IO Bool
testDuality ctxt1 ctxt2 assumptions z s1 s2 = do
    -- Debug: Zeige ursprüngliche Eingabe-Statements
    putStrLn "========== testDuality' CALL =========="
    putStrLn $ "Input s1: " ++ show s1
    putStrLn $ "Input s2: " ++ show s2
    putStrLn $ "Z state: " ++ show z
    
    unsatc <- assumptionsUnsatDual ctxt1 ctxt2 assumptions -- BRANCH-CUT-OFF
    if unsatc then do
        putStrLn "BRANCH-CUT-OFF: assumptions unsatisfiable, returning True"
        return True 
    else do
        let st1 = simplification s1
        let st2 = simplification s2
        
        -- Debug: Zeige vereinfachte Statements
        putStrLn $ "After simplification st1: " ++ show st1
        putStrLn $ "After simplification st2: " ++ show st2
        
        case (st1, st2) of -- simplification and normalization
            (Skip, Skip) -> do
                putStrLn "RULE: ATOM-DUAL-SKIP - both statements are Skip"
                return True -- ATOM-DUAL-SKIP

            (Sequence (Send x) s1, Sequence (Receive y) s2) -> do -- DUAL-SEND-RECV
                case Map.lookup x z of 
                    Just Open -> 
                        case Map.lookup y z of
                            Just Open -> do
                                res <- testDuality ctxt1 ctxt2 assumptions z s1 s2
                                return (x == y && res)
                            Just External -> testDuality ctxt1 ctxt2 assumptions z (Sequence (Send x) s1) s2 -- EXT-RECV
                            _ -> return False -- auf frischen Kanälen darf nicht einfach so eine kanalaktion ausgeführt werden
                    Just External -> testDuality ctxt1 ctxt2 assumptions z s1 (Sequence (Receive y) s2) -- EXT-SEND
                    _ -> return False -- auf frischen kanälen darf nicht einfach so eine kanalaktion ausgeführt werden
            (Sequence (Receive x) s1, Sequence (Send y) s2) -> do -- DUAL-SEND-RECV SYMMETRIC
                case Map.lookup x z of 
                    Just Open -> 
                        case Map.lookup y z of
                            Just Open -> do
                                res <- testDuality ctxt1 ctxt2 assumptions z s1 s2
                                return (x == y && res)
                            Just External -> testDuality ctxt1 ctxt2 assumptions z (Sequence (Receive x) s1) s2
                            _ -> return False
                    Just External -> testDuality ctxt1 ctxt2 assumptions z s1 (Sequence (Send y) s2)
                    _ -> return False

            (Sequence (End x) s1, s2) -> do -- CLOSE
                case Map.lookup x z of
                    Just Closed -> return False
                    _ -> do
                        let z' = Map.insert x Closed z
                         in testDuality ctxt1 ctxt2 assumptions z' s1 s2
            
            (Sequence (Receive x) s1, s2) -> do
                case Map.lookup x z of
                    Just Closed -> testDuality ctxt1 ctxt2 assumptions z s1 s2 -- CLOSED-RECV
                    Just External -> testDuality ctxt1 ctxt2 assumptions z s1 s2 -- EXT-RECV
                    _ -> return False

            (Sequence (Send x) s1, s2) -> do
                case Map.lookup x z of
                    Just External -> testDuality ctxt1 ctxt2 assumptions z s1 s2 -- EXT-SEND
                    _ -> return False

            (Make x _ s1, s2) -> do -- MAKE
                putStrLn $ "RULE: MAKE - Make " ++ show x
                if Map.notMember x z then do
                    let z' = Map.insert x Fresh z
                    putStrLn $ "Added " ++ show x ++ " as Fresh to Z"
                    testDuality ctxt1 ctxt2 assumptions z' s1 s2
                else do
                    putStrLn $ "Channel " ++ show x ++ " already exists in Z, returning False"
                    return False

            (Sequence (If e s1 s2) s0, s) -> do -- COND-SPLIT
                putStrLn $ "RULE: COND-SPLIT - If statement in first position"
                putStrLn $ "Condition: " ++ show e
                r1 <- testDuality ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) z (Sequence s1 s0) s
                r2 <- testDuality ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) z (Sequence s2 s0) s
                let result = (r1 && r2)
                putStrLn $ "COND-SPLIT result: " ++ show result
                return result
                
            (Sequence (Go s0) s1, s2) -> do -- GO
                putStrLn $ "RULE: GO - Go statement detected"
                putStrLn $ "Go body: " ++ show s0
                putStrLn $ "Continuation: " ++ show s1
                let ch0 = channels s0
                    ch1 = channels s1
                    zinternal = initZinternal z ch0 ch1
                putStrLn $ "Internal channels: " ++ show ch0 ++ " intersect " ++ show ch1
                -- s0 ≍ s1
                internalOk <- testDuality ctxt1 ctxt2 assumptions zinternal (addSkip s0) s1
                putStrLn $ "Internal duality check result: " ++ show internalOk
                if not internalOk then do
                    putStrLn "GO: Internal duality failed, returning False"
                    return False
                else do
                    let zexternal = initZexternal z
                    putStrLn $ "GO: Before initZexternal: " ++ show z
                    putStrLn $ "GO: After initZexternal: " ++ show zexternal
                    putStrLn "GO: Internal duality OK, checking external"
                    -- (s0 || s1) ≍ s2
                    externalCheck ctxt1 ctxt2 assumptions zexternal (addSkip s0) s1 s2
            

            (s1, Sequence (Receive x) s2) -> do
                putStrLn $ "RULE: CLOSED/EXT-RECV SYMMETRIC - " ++ show s1 ++ " vs Receive " ++ show x
                case Map.lookup x z of
                    Just Closed -> do
                        putStrLn $ "Channel " ++ show x ++ " is Closed, recursing"
                        testDuality ctxt1 ctxt2 assumptions z s1 s2 -- CLOSED-RECV SYMMETRIC
                    Just External -> do
                        putStrLn $ "Channel " ++ show x ++ " is External, recursing"
                        testDuality ctxt1 ctxt2 assumptions z s1 s2 -- EXT-RECV SYMMETRIC
                    _ -> do
                        putStrLn $ "Channel " ++ show x ++ " neither Closed nor External, returning False"
                        return False
            (s1, Sequence (End x) s2) -> do -- CLOSE SYMMETRIC
                putStrLn $ "RULE: CLOSE SYMMETRIC - " ++ show s1 ++ " vs End " ++ show x
                case Map.lookup x z of
                    Just Closed -> do
                        putStrLn $ "Channel " ++ show x ++ " already Closed, returning False"
                        return False
                    _ -> do
                        let z' = Map.insert x Closed z
                        putStrLn $ "Closing channel " ++ show x ++ ", updated Z: " ++ show z'
                        testDuality ctxt1 ctxt2 assumptions z' s1 s2
            (s1, Sequence (Send x) s2) -> do -- EXT-SEND SYMMETRIC
                putStrLn $ "RULE: EXT-SEND SYMMETRIC - " ++ show s1 ++ " vs Send " ++ show x
                case Map.lookup x z of
                    Just External -> do
                        putStrLn $ "Channel " ++ show x ++ " is External, recursing"
                        testDuality ctxt1 ctxt2 assumptions z s1 s2 -- EXT-SEND SYMMETRIC
                    _ -> do
                        putStrLn $ "Channel " ++ show x ++ " not External, returning False"
                        return False
            (s1, Make x _ s2) -> do -- MAKE SYMMETRIC
                putStrLn $ "RULE: MAKE SYMMETRIC - " ++ show s1 ++ " vs Make " ++ show x
                if Map.notMember x z then do
                    let z' = Map.insert x Fresh z
                    putStrLn $ "Channel " ++ show x ++ " not in Z, adding as Fresh: " ++ show z'
                    testDuality ctxt1 ctxt2 assumptions z' s1 s2
                else do
                    putStrLn $ "Channel " ++ show x ++ " already in Z, returning False"
                    return False

            (s0, Sequence (If e s1 s2) s) -> do -- COND-SPLIT--SYMMETRIC
                putStrLn $ "RULE: COND-SPLIT SYMMETRIC - " ++ show s0 ++ " vs If " ++ show e
                putStrLn "Testing both branches of conditional..."
                r1 <- testDuality ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) z s0 (Sequence s1 s)
                putStrLn $ "First branch result: " ++ show r1
                r2 <- testDuality ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) z s0 (Sequence s2 s)
                putStrLn $ "Second branch result: " ++ show r2
                let result = r1 && r2
                putStrLn $ "COND-SPLIT SYMMETRIC final result: " ++ show result
                return result
            _ -> do -- Fallback
                putStrLn $ "FALLBACK CASE - No rule matched for: " ++ show (fst (st1, st2)) ++ " vs " ++ show (snd (st1, st2))
                putStrLn $ "Current Z state: " ++ show z
                return False 

-- Helper function for checking single duality steps
singleCheck :: [Expr] -> Z -> Statement -> Statement -> ([Expr], Z, Statement, Statement, Bool)
singleCheck assumptions z stmt1 stmt2 = case (stmt1, stmt2) of
    (Sequence (Send x) s1, Sequence (Receive y) s2) ->
        if x == y then
            case Map.lookup x z of
                Just External -> (assumptions, z, s1, s2, True) -- EXT-SEND
                _ -> (assumptions, z, s1, s2, True) -- DUAL-SEND-RECV
        else (assumptions, z, stmt1, stmt2, False)
    (Sequence (Receive x) s1, Sequence (Send y) s2) ->
        if x == y then
            case Map.lookup x z of
                Just External -> (assumptions, z, s1, s2, True) -- EXT-SEND SYMMETRIC
                _ -> (assumptions, z, s1, s2, True) -- DUAL-SEND-RECV SYMMETRIC
        else (assumptions, z, stmt1, stmt2, False)
    (Sequence (Send x) s1, s2) -> case Map.lookup x z of -- EXT-SEND
        Just External -> (assumptions, z, s1, s2, True)
        _ -> (assumptions, z, stmt1, stmt2, False)
    (s1, Sequence (Send x) s2) -> case Map.lookup x z of -- EXT-SEND SYMMETRIC
        Just External -> (assumptions, z, s1, s2, True)
        _ -> (assumptions, z, stmt1, stmt2, False)
    (Sequence (End x) s1, s2) -> case Map.lookup x z of -- CLOSE
        Just Closed -> (assumptions, z, stmt1, stmt2, False)
        _ -> (assumptions, Map.insert x Closed z, s1, s2, True)
    (s1, Sequence (End x) s2) -> case Map.lookup x z of -- CLOSE SYMMETRIC
        Just Closed -> (assumptions, z, stmt1, stmt2, False)
        _ -> (assumptions, Map.insert x Closed z, s1, s2, True)
    (Sequence (Receive x) s1, s2) -> case Map.lookup x z of
        Just Closed -> (assumptions, z, s1, s2, True) -- CLOSED-RECEIVE
        Just External -> (assumptions, z, s1, s2, True) -- EXT-RECEIVE
        _ -> (assumptions, z, stmt1, stmt2, False)
    (s1, Sequence (Receive x) s2) -> case Map.lookup x z of  -- SYMMETRIC
        Just Closed -> (assumptions, z, s1, s2, True) -- CLOSED-RECEIVE
        Just External -> (assumptions, z, s1, s2, True) -- EXT-RECEIVE
        _ -> (assumptions, z, stmt1, stmt2, False)
    (Make x _ s1, s2) -> case Map.lookup x z of -- MAKE
        Nothing -> (assumptions, Map.insert x Fresh z, s1, s2, True)
        _ -> (assumptions, z, stmt1, stmt2, False)
    (s1, Make x _ s2) -> case Map.lookup x z of -- MAKE SYMMETRIC
        Nothing -> (assumptions, Map.insert x Fresh z, s1, s2, True)
        _ -> (assumptions, z, stmt1, stmt2, False)
    _ -> (assumptions, z, stmt1, stmt2, False) -- No rule applies

externalCheck :: Context -> Context -> [Expr] -> Z -> Statement -> Statement -> Statement -> IO Bool
externalCheck ctxt1 ctxt2 assumptions z s0 s1 s2 = do
  -- SIMPLIFICATION
    let s0' = simplification s0
    let s1' = simplification s1
    let s2' = simplification s2
    -- ATOM-SKIP
    if s0' == Skip && s1' == Skip && s2' == Skip then return True
    else
        -- COND-SPLIT-S0 + BRANCH-CUT-OFF
        case s0' of
            Sequence (If e a b) rest -> do
                let ass1 = assumptions ++ [e]
                let ass2 = assumptions ++ [ENot e]
                unsat1 <- assumptionsUnsatDual ctxt1 ctxt2 ass1
                unsat2 <- assumptionsUnsatDual ctxt1 ctxt2 ass2
                r1 <- if unsat1
                    then return True
                    else externalCheck ctxt1 ctxt2 ass1 z (Sequence a rest) s1' s2'
                r2 <- if unsat2
                    then return True
                    else externalCheck ctxt1 ctxt2 ass2 z (Sequence b rest) s1' s2'
                return (r1 && r2)
            _ -> case s1' of -- COND-SPLIT-S1 + BRANCH-CUT-OFF
                Sequence (If e a b) rest -> do
                    let ass1 = assumptions ++ [e]
                    let ass2 = assumptions ++ [ENot e]
                    unsat1 <- assumptionsUnsatDual ctxt1 ctxt2 ass1
                    unsat2 <- assumptionsUnsatDual ctxt1 ctxt2 ass2
                    r1 <- if unsat1
                        then return True
                        else externalCheck ctxt1 ctxt2 ass1 z  s0' (Sequence a rest) s2'
                    r2 <- if unsat2
                        then return True
                        else externalCheck ctxt1 ctxt2 ass2 z  s0' (Sequence b rest) s2'
                    return (r1 && r2)
                _ -> case s2' of -- COND-SPLIT-S2 + BRANCH-CUT-OFF
                    Sequence (If e a b) rest -> do
                        let ass1 = assumptions ++ [e]
                        let ass2 = assumptions ++ [ENot e]
                        unsat1 <- assumptionsUnsatDual ctxt1 ctxt2 ass1
                        unsat2 <- assumptionsUnsatDual ctxt1 ctxt2 ass2
                        r1 <- if unsat1
                            then return True
                            else externalCheck ctxt1 ctxt2 ass1 z  s0'  s1' (Sequence a rest)
                        r2 <- if unsat2
                            then return True
                            else externalCheck ctxt1 ctxt2 ass2 z  s0' s1' (Sequence b rest)                    
                        return (r1 && r2)
                    _ -> case s0' of
                        Sequence (Go a) rest -> do
                            let zext = initZexternal z
                            r1 <- externalCheck ctxt1 ctxt2 assumptions zext rest s1' s2'
                            r2 <- externalCheck ctxt1 ctxt2 assumptions zext a s1' s2'
                            return (r1 || r2)
                        _ -> case s1' of
                            Sequence (Go a) rest -> do
                                let zext = initZexternal z
                                r1 <- externalCheck ctxt1 ctxt2 assumptions zext s0' rest s2'
                                r2 <- externalCheck ctxt1 ctxt2 assumptions zext s0' a s2'    
                                return (r1 || r2)   
                            _ -> case singleCheck assumptions z s0' s2' of
                                (ass', z', s0next, s2next, True) -> externalCheck ctxt1 ctxt2 ass' z' s0next s1' s2next
                                _ -> case singleCheck assumptions z s1' s2' of 
                                    (ass', z', s1next, s2next, True) -> externalCheck ctxt1 ctxt2 ass' z' s0' s1next s2next
                                    _ -> return False

-- Helper function init-Z-internal
initZinternal :: Z -> [VarName] -> [VarName] -> Z
initZinternal z ch0 ch1 = Map.mapWithKey updateState z
  where
    updateState c state
      | state == Fresh && c `elem` (ch0 `List.intersect` ch1) = Open
      | state == Fresh && c `elem` ((ch0 List.\\ ch1) ++ (ch1 List.\\ ch0)) = Fresh
      | state /= Fresh = External
      | otherwise = state

-- Helper function init-Z-external  
initZexternal :: Z -> Z
initZexternal z = Map.map updateState z
  where
    updateState Fresh = External
    updateState state = state

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

-- extracts all channels of a statement (not only free ones)
channels :: Statement -> [VarName]
channels st = case st of
  Send x        -> [x]
  Receive x    -> [x]
  End x        -> [x]
  Make x _ s   -> x : channels s
  Sequence a b -> channels a ++ channels b
  If _ a b     -> channels a ++ channels b
  Go s         -> channels s
  _            -> []

internalST :: Statement -> [VarName] -> Statement
internalST st locals = case st of
    Send x -> if x `elem` locals then Send x else Skip
    Receive x -> if x `elem` locals then Receive x else Skip
    End x -> if x `elem` locals then End x else Skip
    Sequence s1 s2 -> Sequence (internalST s1 locals) (internalST s2 locals)
    Make x t s -> Make x t (internalST s locals)
    Go s -> Go (internalST s locals)
    If e s1 s2 -> let s1' = internalST s1 locals
                      s2' = internalST s2 locals
                    in if s1' == Skip && s2' == Skip then Skip else If e s1' s2'
    _ -> Skip

externalST :: Statement -> [VarName] -> Statement
externalST st locals = case st of
    Send x -> if x `notElem` locals then Send x else Skip
    Receive x -> if x `notElem` locals then Receive x else Skip
    End x -> if x `notElem` locals then End x else Skip
    Sequence s1 s2 -> Sequence (externalST s1 locals) (externalST s2 locals)
    Make x t s -> Make x t (externalST s locals)
    Go s -> Go (externalST s locals)
    If e s1 s2 -> let s1' = externalST s1 locals
                      s2' = externalST s2 locals
                    in if s1' == Skip && s2' == Skip then Skip else If e s1' s2'
    _ -> Skip

initialZ :: Z -> VarDecs -> Context -> Z
initialZ z [] _ = z
initialZ z (dec1:rest) ctxt = case dec1 of
    (x, TChan _) -> case Map.lookup x ctxt of --initialZ (Map.insert x Open z) rest
                        Just (TChan _, AChan (ChannelID id)) -> initialZ (Map.insert (VarName id) Open z) rest ctxt
                        _ -> initialZ z rest ctxt
    _ -> initialZ z rest ctxt
