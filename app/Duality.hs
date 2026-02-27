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
testDuality ctxt1 ctxt2 assumptions z stmt1 stmt2 = do
    -- Debug: Zeige ursprüngliche Eingabe-Statements
    putStrLn "========== testDuality' CALL =========="
    
    unsatc <- assumptionsUnsatDual ctxt1 ctxt2 assumptions -- BRANCH-CUT-OFF
    if unsatc then do
        putStrLn "BRANCH-CUT-OFF: assumptions unsatisfiable, returning True"
        return True 
    else do
        let st1 = simplification stmt1
        let st2 = simplification stmt2
        
        -- Debug: Zeige vereinfachte Statements
        putStrLn $ "st1: " ++ show st1
        putStrLn $ "st2: " ++ show st2
        putStrLn $ "Z state: " ++ show z
        
        case (st1, st2) of -- simplification and normalization
            (Skip, Skip) -> do
                putStrLn "RULE: ATOM-DUAL-SKIP"
                return True -- ATOM-DUAL-SKIP

            (Sequence (Send x) s1, Sequence (Receive y) s2) -> do -- DUAL-SEND-RECV
                putStrLn $ "RULE: Check for DUAL-SEND-RECV"
                case Map.lookup x z of 
                    Just Open -> 
                        case Map.lookup y z of
                                Just Open -> do -- nur, wenn der Kanal offen ist, darf DUAL-SEND-RECV angewendet werden
                                    putStrLn "RULE: DUAL-SEND-RECV"
                                    res <- testDuality ctxt1 ctxt2 assumptions z s1 s2
                                    return (x == y && res)
                                Just External -> do
                                    putStrLn "RULE: EXT-RECV SYMMETRIC"
                                    testDuality ctxt1 ctxt2 assumptions z (Sequence (Send x) s1) s2
                                _ -> return False
                    Just External -> do
                        putStrLn "RULE: EXT-SEND"
                        testDuality ctxt1 ctxt2 assumptions z s1 (Sequence (Receive y) s2)
                    _ -> return False -- auf frischen Kanälen darf keine Kanalaktion ausgeführt werden

            (Sequence (Receive x) s1, Sequence (Send y) s2) -> do -- DUAL-SEND-RECV SYMMETRIC
                putStrLn $ "RULE: Check for DUAL-SEND-RECV SYMMETRIC"
                case Map.lookup x z of 
                    Just Open -> 
                        case Map.lookup y z of
                            Just Open -> do
                                putStrLn "RULE: DUAL-SEND-RECV SYMMETRIC"
                                res <- testDuality ctxt1 ctxt2 assumptions z s1 s2
                                return (x == y && res)
                            Just External -> do
                                putStrLn $ "RULE: EXT-SEND SYMMETRIC"
                                testDuality ctxt1 ctxt2 assumptions z (Sequence (Receive x) s1) s2
                            _ -> return False
                    Just External -> do
                        putStrLn "RULE: EXT-RECV"
                        testDuality ctxt1 ctxt2 assumptions z s1 (Sequence (Send y) s2)
                    _ -> return False

            -- Wenn beide Send oder beide Receive, checke ob bei einem der beiden EXT-RECV/SEND angewendet werden kann
            (Sequence (Send x) s1, Sequence (Send y) s2) -> do
                putStrLn $ "RULE: Both sides Send - checking " ++ show x ++ " and " ++ show y ++ " for external"
                let xExternal = Map.lookup x z == Just External
                let yExternal = Map.lookup y z == Just External
                case (xExternal, yExternal) of
                    (True, _) -> do
                        putStrLn $ "RULE: EXT-SEND LEFT" ++ show x
                        testDuality ctxt1 ctxt2 assumptions z s1 (Sequence (Send y) s2)
                    (_, True) -> do
                        putStrLn $ "RULE: EXT-SEND RIGHT" ++ show y
                        testDuality ctxt1 ctxt2 assumptions z (Sequence (Send x) s1) s2
                    (False, False) -> checkOtherRules (Sequence (Send x) s1) (Sequence (Send y) s2)
                    
            (Sequence (Receive x) s1, Sequence (Receive y) s2) -> do
                putStrLn $ "RULE: Both sides Receive - checking " ++ show x ++ " and " ++ show y ++ " for external/closed"
                let xSkippable = Map.lookup x z == Just External || Map.lookup x z == Just Closed
                let ySkippable = Map.lookup y z == Just External || Map.lookup y z == Just Closed
                case (xSkippable, ySkippable) of
                    (True, _) -> do
                        putStrLn $ "RULE: EXT/CLOSED-RECV LEFT" ++ show x
                        testDuality ctxt1 ctxt2 assumptions z s1 (Sequence (Receive y) s2)
                    (_, True) -> do
                        putStrLn $ "RULE: EXT/CLOSED-RECV RIGHT" ++ show y
                        testDuality ctxt1 ctxt2 assumptions z (Sequence (Receive x) s1) s2
                    (False, False) -> checkOtherRules (Sequence (Receive x) s1) (Sequence (Receive y) s2)

            -- Links weder Send noch Recv, rechts aber ein Send oder Recv -> checken ob EXT-rule angewendet werden kann
            (s1, Sequence (Send x) s2) -> do
                putStrLn $ "RULE: Checking right Send " ++ show x ++ " for external"
                case Map.lookup x z of
                    Just External -> do
                        putStrLn $ "RULE: EXT-SEND RIGHT" ++ show x
                        testDuality ctxt1 ctxt2 assumptions z s1 s2 
                    _ -> checkOtherRules s1 (Sequence (Send x) s2)
                    
            (s1, Sequence (Receive x) s2) -> do
                putStrLn $ "RULE: Checking right Receive " ++ show x ++ " for external/closed"
                case Map.lookup x z of
                    Just External -> do
                        putStrLn $ "RULE: EXT-RECV RIGHT" ++ show x
                        testDuality ctxt1 ctxt2 assumptions z s1 s2 
                    Just Closed -> do
                        putStrLn $ "RULE: CLOSED-RECV RIGHT" ++ show x
                        testDuality ctxt1 ctxt2 assumptions z s1 s2 
                    _ -> checkOtherRules s1 (Sequence (Receive x) s2)

            -- Rechts weder Send noch Recv, links aber ein Send oder Recv -> checken ob EXT-rule angewendet werden kann  
            (Sequence (Send x) s1, s2) -> do
                putStrLn $ "RULE: Checking left Send " ++ show x ++ " for external"
                case Map.lookup x z of
                    Just External -> do
                        putStrLn $ "RULE: EXT-SEND LEFT" ++ show x
                        testDuality ctxt1 ctxt2 assumptions z s1 s2 
                    _ -> checkOtherRules (Sequence (Send x) s1) s2
                    
            (Sequence (Receive x) s1, s2) -> do
                putStrLn $ "RULE: Checking left Receive " ++ show x ++ " for external/closed"
                case Map.lookup x z of
                    Just External -> do
                        putStrLn $ "RULE: EXT-RECV LEFT" ++ show x
                        testDuality ctxt1 ctxt2 assumptions z s1 s2 
                    Just Closed -> do
                        putStrLn $ "RULE: CLOSED-RECV LEFT" ++ show x
                        testDuality ctxt1 ctxt2 assumptions z s1 s2 
                    _ -> checkOtherRules (Sequence (Receive x) s1) s2

            -- alle anderen Regeln in checkOtherRules
            _ -> checkOtherRules st1 st2   
  where
    checkOtherRules st1 st2 = case (st1, st2) of
            (Sequence (End x) s1, s2) -> do -- CLOSE
                putStrLn $ "RULE: CLOSE " ++ show  x
                case Map.lookup x z of
                    Just Closed -> do
                        putStrLn $ "Channel already Closed, returning False"
                        return False
                    _ -> do
                        let z' = Map.insert x Closed z
                        putStrLn $ "Closing channel " ++ show x ++ ", updated Z: " ++ show z'
                        testDuality ctxt1 ctxt2 assumptions z' s1 s2
                         
            (s1, Sequence (End x) s2) -> do -- CLOSE SYMMETRIC
                putStrLn $ "RULE: CLOSE SYMMETRIC - " ++ show x
                case Map.lookup x z of
                    Just Closed -> do
                        putStrLn $ "Channel already Closed, returning False"
                        return False
                    _ -> do
                        let z' = Map.insert x Closed z
                        putStrLn $ "Closing channel " ++ show x ++ ", updated Z: " ++ show z'
                        testDuality ctxt1 ctxt2 assumptions z' s1 s2

            (Make x _ s1, s2) -> do -- MAKE
                putStrLn "RULE: MAKE"
                if Map.notMember x z then do
                    let z' = Map.insert x Fresh z
                    putStrLn $ "Added " ++ show x ++ " as Fresh to Z"
                    testDuality ctxt1 ctxt2 assumptions z' s1 s2
                else do
                    putStrLn $ "Channel " ++ show x ++ " already exists in Z, returning False"
                    return False

            (s1, Make x _ s2) -> do -- MAKE SYMMETRIC
                putStrLn "RULE: MAKE SYMMETRIC"
                if Map.notMember x z then do
                    let z' = Map.insert x Fresh z
                    putStrLn $ "Added " ++ show x ++ " as Fresh to Z"
                    testDuality ctxt1 ctxt2 assumptions z' s1 s2
                else do
                    putStrLn $ "Channel " ++ show x ++ " already in Z, returning False"
                    return False

            (Sequence (If e s1 s2) s0, s) -> do -- COND-SPLIT
                putStrLn "RULE: COND-SPLIT - If statement in first position"
                putStrLn $ "Condition: " ++ show e
                r1 <- testDuality ctxt1 ctxt2 (assumptions ++ [EBinOp Eq e (EBool True)]) z (Sequence s1 s0) s
                r2 <- testDuality ctxt1 ctxt2 (assumptions ++ [EBinOp Eq (ENot e) (EBool True)]) z (Sequence s2 s0) s
                let result = r1 && r2
                putStrLn $ "COND-SPLIT result: " ++ show result
                return result
                
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
                
            -- GO rules need to be before EXT rules to prevent conflicts
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
                    -- (s0;skip || s1) ≍ s2
                    externalCheck ctxt1 ctxt2 assumptions zexternal [(addSkip s0), s1] [s2]

            -- GO rule needs to be before EXT-SEND/EXT-RECV to prevent conflicts
            (s0, Sequence (Go s1) s2) -> do -- GO SYMMETRIC
                putStrLn $ "RULE: GO SYMMETRIC - " ++ show s0 ++ " vs Go " ++ show s1
                putStrLn $ "Go body: " ++ show s1
                putStrLn $ "Continuation: " ++ show s2
                let ch0 = channels s1
                    ch1 = channels s2
                    zinternal = initZinternal z ch0 ch1
                putStrLn $ "Internal channels: " ++ show ch0 ++ " intersect " ++ show ch1
                -- s1 ≍ s2
                internalOk <- testDuality ctxt1 ctxt2 assumptions zinternal (addSkip s1) s2
                putStrLn $ "Internal duality check result: " ++ show internalOk
                if not internalOk then do
                    putStrLn "GO SYMMETRIC: Internal duality failed, returning False"
                    return False
                else do
                    let zexternal = initZexternal z
                    putStrLn $ "GO SYMMETRIC: Before initZexternal: " ++ show z
                    putStrLn $ "GO SYMMETRIC: After initZexternal: " ++ show zexternal
                    putStrLn "GO SYMMETRIC: Internal duality OK, checking external"
                    -- s0 ≍ (s1;skip || s2)
                    externalCheck ctxt1 ctxt2 assumptions zexternal [s0] [addSkip s1, s2]
            
            _ -> do -- Fallback
                putStrLn $ "FALLBACK CASE - No rule matched for: " ++ show (fst (st1, st2)) ++ " vs " ++ show (snd (st1, st2))
                putStrLn $ "Current Z state: " ++ show z
                return False

externalCheck :: Context -> Context -> [Expr] -> Z -> [Statement] -> [Statement] -> IO Bool
externalCheck ctxt1 ctxt2 assumptions z leftProcs rightProcs = do
    putStrLn "========== externalCheck CALL =========="
    putStrLn $ "Left processes: " ++ show leftProcs
    putStrLn $ "Right processes: " ++ show rightProcs
    putStrLn $ "Z state: " ++ show z
    
    -- Check for branch cut-off first
    unsatc <- assumptionsUnsatDual ctxt1 ctxt2 assumptions
    if unsatc then do
        putStrLn "BRANCH-CUT-OFF: assumptions unsatisfiable in externalCheck"
        return True
    else do
        -- SIMPLIFICATION
        let leftSimplified = map simplification leftProcs
        let rightSimplified = map simplification rightProcs
        externalCheckStep ctxt1 ctxt2 assumptions z leftSimplified rightSimplified

-- Main logic for external check with parallel processes
externalCheckStep :: Context -> Context -> [Expr] -> Z -> [Statement] -> [Statement] -> IO Bool
externalCheckStep ctxt1 ctxt2 assumptions z leftProcs rightProcs = do
    -- Remove Skip statements
    let leftFiltered = filter (not . isSkip) leftProcs
    let rightFiltered = filter (not . isSkip) rightProcs
    
    case (leftFiltered, rightFiltered) of
        ([], []) -> do -- ATOM-SKIP
            putStrLn "EXTERNAL: Both sides empty, returning True"
            return True
            
        -- Try to progress from left side first
        (l:ls, rs) -> do
            putStrLn $ "EXTERNAL: Processing left statement: " ++ show l
            canProgress <- progressLeftStatement ctxt1 ctxt2 assumptions z l ls rs
            if canProgress then
                return True
            else do
                putStrLn $ "EXTERNAL: Cannot progress left statement: " ++ show l
                return False
                
        -- If left side is empty, try to progress from right side
        ([], r:rs) -> do
            putStrLn $ "EXTERNAL: Processing right statement: " ++ show r
            canProgress <- progressRightStatement ctxt1 ctxt2 assumptions z [] r rs
            if canProgress then
                return True
            else do
                putStrLn $ "EXTERNAL: Cannot progress right statement: " ++ show r
                return False

progressLeftStatement :: Context -> Context -> [Expr] -> Z -> Statement -> [Statement] -> [Statement] -> IO Bool
progressLeftStatement ctxt1 ctxt2 assumptions z stmt leftRest rightProcs = 
    -- SIMPLIFICATION
    case simplification stmt of
        Sequence (Send x) s' -> do
            case Map.lookup x z of
                Just Open -> do
                    -- DUAL-SEND-RECV
                    putStrLn $ "DUAL-SEND-RECV: " ++ show x
                    result <- tryFindMatchingReceive ctxt1 ctxt2 assumptions z x s' leftRest rightProcs
                    if result then return True
                    else return False
                Just External -> do
                    -- EXT-SEND
                    putStrLn $ "EXTERNAL: Left external send " ++ show x
                    externalCheckStep ctxt1 ctxt2 assumptions z (s':leftRest) rightProcs
                -- no channel action on fresh Channels allowed
                _ -> return False

        Sequence (Receive x) s' -> do
            case Map.lookup x z of
                Just Open -> do
                    -- DUAL-SEND-RECV SYMMETRIC
                    putStrLn $ "DUAL-SEND-RECV SYMMETRIC: " ++ show x
                    result <- tryFindMatchingSend ctxt1 ctxt2 assumptions z x s' leftRest rightProcs
                    if result then return True
                    else return False
                Just External -> do
                    -- EXT-RECV 
                    putStrLn $ "EXTERNAL: Left external receive " ++ show x
                    externalCheckStep ctxt1 ctxt2 assumptions z (s':leftRest) rightProcs
                Just Closed -> do
                    -- CLOSED-RECV
                    putStrLn $ "CLOSED-RECV: Left closed receive " ++ show x
                    externalCheckStep ctxt1 ctxt2 assumptions z (s':leftRest) rightProcs
                _ -> return False

        -- Close actions
        Sequence (End x) s' -> do
            case Map.lookup x z of
                -- Double close not allowed
                Just Closed -> return False
                _ -> do
                    -- CLOSE
                    putStrLn $ "CLOSE: Left close " ++ show x
                    let z' = Map.insert x Closed z
                    externalCheckStep ctxt1 ctxt2 assumptions z' (s':leftRest) rightProcs

        -- Make actions
        Make x _ s' -> do
            -- MAKE
            if Map.notMember x z then do
                putStrLn $ "MAKE: Left make " ++ show x
                let z' = Map.insert x Fresh z
                externalCheckStep ctxt1 ctxt2 assumptions z' (s':leftRest) rightProcs
            else return False

        -- COND-SPLIT
        Sequence (If e s1 s2) s' -> do
            putStrLn $ "IF: Left if statement - applying Cond-Split"
            let assumptions1 = assumptions ++ [EBinOp Eq e (EBool True)]
            let assumptions2 = assumptions ++ [EBinOp Eq (ENot e) (EBool True)]
            result1 <- externalCheckStep ctxt1 ctxt2 assumptions1 z (Sequence s1 s':leftRest) rightProcs
            result2 <- externalCheckStep ctxt1 ctxt2 assumptions2 z (Sequence s2 s':leftRest) rightProcs
            return (result1 && result2)

        -- GO 
        Sequence (Go s0) s' -> do
            putStrLn $ "GO: Left go statement"
            let ch0 = channels s0
            let ch1 = channels s'
            let zinternal = initZinternal z ch0 ch1
            -- Check internal duality: s0 ≍ s'
            internalOk <- testDuality ctxt1 ctxt2 assumptions zinternal (addSkip s0) s'
            if not internalOk then
                return False
            else do
                -- Add both parallel processes to left side
                let zexternal = initZexternal z
                externalCheckStep ctxt1 ctxt2 assumptions zexternal (addSkip s0:s':leftRest) rightProcs

        _ -> return False

-- SYMMETRIC cases handled here
progressRightStatement :: Context -> Context -> [Expr] -> Z -> [Statement] -> Statement -> [Statement] -> IO Bool
progressRightStatement ctxt1 ctxt2 assumptions z leftProcs stmt rightRest = 
    -- SIMPLIFICATION
    case simplification stmt of
        Sequence (Receive x) s' -> do
            case Map.lookup x z of
                -- Just Open not needed, case already handled in progressLeftStatement
                Just External -> do
                    -- EXT-RECV SYMMETRIC
                    putStrLn $ "EXTERNAL SYMMETRIC: Right external receive " ++ show x
                    externalCheckStep ctxt1 ctxt2 assumptions z leftProcs (s':rightRest)
                Just Closed -> do
                    -- CLOSED-RECV SYMMETRIC
                    putStrLn $ "CLOSED-RECV SYMMETRIC: Right closed receive " ++ show x
                    externalCheckStep ctxt1 ctxt2 assumptions z leftProcs (s':rightRest)
                _ -> return False

        Sequence (Send x) s' -> do
            case Map.lookup x z of
                Just External -> do
                    -- EXT-SEND SYMMETRIC
                    putStrLn $ "EXTERNAL SYMMETRIC: Right external send " ++ show x
                    externalCheckStep ctxt1 ctxt2 assumptions z leftProcs (s':rightRest)
                _ -> return False

        -- CLOSE SYMMETRIC
        Sequence (End x) s' -> do
            case Map.lookup x z of
                Just Closed -> return False
                _ -> do
                    putStrLn $ "CLOSE SYMMETRIC: Right close " ++ show x
                    let z' = Map.insert x Closed z
                    externalCheckStep ctxt1 ctxt2 assumptions z' leftProcs (s':rightRest)

        -- MAKE SYMMETRIC
        Make x _ s' -> do
            if Map.notMember x z then do
                putStrLn $ "EXTERNAL: Right make " ++ show x
                let z' = Map.insert x Fresh z
                externalCheckStep ctxt1 ctxt2 assumptions z' leftProcs (s':rightRest)
            else return False

        -- If statements (Cond-Split symmetric)
        Sequence (If e s1 s2) s' -> do
            putStrLn "EXTERNAL: Right if statement - applying Cond-Split"
            let assumptions1 = assumptions ++ [EBinOp Eq e (EBool True)]
            let assumptions2 = assumptions ++ [EBinOp Eq (ENot e) (EBool True)]

            result1 <- externalCheckStep ctxt1 ctxt2 assumptions1 z leftProcs (Sequence s1 s':rightRest)  
            result2 <- externalCheckStep ctxt1 ctxt2 assumptions2 z leftProcs (Sequence s2 s':rightRest)

            return (result1 && result2)

        -- Go statements (symmetric)
        Sequence (Go s0) s' -> do
            putStrLn "EXTERNAL: Right go statement"
            let ch0 = channels s0
            let ch1 = channels s'
            let zinternal = initZinternal z ch0 ch1

            -- Check internal duality: s0 ≍ s'  
            internalOk <- testDuality ctxt1 ctxt2 assumptions zinternal (addSkip s0) s'
            if not internalOk then
                return False
            else do
                -- Add both parallel processes to right side
                let zexternal = initZexternal z
                externalCheckStep ctxt1 ctxt2 assumptions zexternal leftProcs (addSkip s0:s':rightRest)

        _ -> return False

-- Try to find matching receive for a send
tryFindMatchingReceive :: Context -> Context -> [Expr] -> Z -> VarName -> Statement -> [Statement] -> [Statement] -> IO Bool
tryFindMatchingReceive ctxt1 ctxt2 assumptions z sendChan sendRest leftRest rightProcs = 
    findAndRemoveMatch rightProcs []
  where
    findAndRemoveMatch [] _ = return False
    findAndRemoveMatch (r:rs) before = case r of
        Sequence (Receive y) r' | sendChan == y ->
            case Map.lookup sendChan z of
                Just Open -> do
                    putStrLn $ "EXTERNAL: Matched send/recv on " ++ show sendChan
                    let newRight = before ++ [r'] ++ rs
                    externalCheckStep ctxt1 ctxt2 assumptions z (sendRest:leftRest) newRight
                _ -> findAndRemoveMatch rs (before ++ [r])
        _ -> findAndRemoveMatch rs (before ++ [r])

-- Try to find matching send for a receive  
tryFindMatchingSend :: Context -> Context -> [Expr] -> Z -> VarName -> Statement -> [Statement] -> [Statement] -> IO Bool
tryFindMatchingSend ctxt1 ctxt2 assumptions z recvChan recvRest leftRest rightProcs =
    findAndRemoveMatch rightProcs []
  where
    findAndRemoveMatch [] _ = return False
    findAndRemoveMatch (r:rs) before = case r of
        Sequence (Send y) r' | recvChan == y ->
            case Map.lookup recvChan z of
                Just Open -> do
                    putStrLn $ "EXTERNAL: Matched recv/send on " ++ show recvChan
                    let newRight = before ++ [r'] ++ rs  
                    externalCheckStep ctxt1 ctxt2 assumptions z (recvRest:leftRest) newRight
                _ -> findAndRemoveMatch rs (before ++ [r])
        _ -> findAndRemoveMatch rs (before ++ [r])

-- Helper function to check if a statement is Skip
isSkip :: Statement -> Bool
isSkip Skip = True
isSkip _ = False

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
    (x, TChan _) -> case Map.lookup x ctxt of
                        Just (TChan _, AChan (ChannelID id)) -> initialZ (Map.insert (VarName id) Open z) rest ctxt
                        _ -> initialZ z rest ctxt
    _ -> initialZ z rest ctxt
