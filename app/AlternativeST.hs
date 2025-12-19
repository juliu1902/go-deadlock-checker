module AlternativeST where

import qualified Data.Map      as Map (Map, delete, deleteMin, empty, fromList,
                                       insert, lookup, lookupMin, map,
                                       mapWithKey, toList, union)
import           Datastructure

-- evaluates conditions and considers the context when there is a send/receive
alternativeSTNaming :: Context -> Int -> Statement -> IO (Statement, Context, Int)
alternativeSTNaming ctxt state st =
  case st of
    Declare v t -> return (Declare v t, (Map.insert v (t, AUnknown) ctxt), state)
    Make v chant -> do
      let freshId = ChannelID ("oid" ++ show state)
       in return (Make v chant, updateOneAV v (AChan freshId) ctxt, state+1)
    Sequence s1 s2 -> do
      (s1', ctxt1, state1) <- alternativeSTNaming ctxt state s1
      (s2', ctxt2, state2) <- alternativeSTNaming ctxt1 state1 s2
      return (Sequence s1' s2', ctxt2, state)
    If e s1 s2 -> do
      (s1', ctxt1, st1) <- alternativeSTNaming ctxt state s1
      (s2', ctxt2, st2) <-  alternativeSTNaming ctxt state s2
      res1 <- resolveIf (If (evalExpr e ctxt) s1 s2) ctxt
      let e' = evalExpr e ctxt
          ctxtMerged = (mergeIfContexts e' ctxt1 ctxt2)
       in if res1 == s1 then return (s1', ctxt1, st1) else if res1 == s2 then return (s2', ctxt2, st2) else if st1 > st2 then return (If e' s1' s2', ctxtMerged, st1) else return (If e' s1' s2', ctxtMerged, st2)
    Assign var (EVar x) ->
      case lookupAV x ctxt of
        ATerm e -> return (Assign var e, (updateOneAV var (ATerm e) ctxt), state) -- var bekommt den Term der x definiert
        AIf e av1 av2 ->
          return (Assign var (EVar x), (updateOneAV var (AIf e av1 av2) ctxt), state) -- var bekommt die AIf condition von x
        _ -> return (Assign var (EVar x), (updateOneAV var (ATerm (EVar x)) ctxt), state)
    Assign var (EBinOp op e1 e2) ->
      let e1' = evalExpr e1 ctxt
          e2' = evalExpr e2 ctxt
       in return ( Assign var (EBinOp op e1' e2')
          , (updateOneAV var (ATerm (EBinOp op e1' e2')) ctxt), state)
    Assign var (ENot expr) ->
      let expr' = evalExpr (ENot expr) ctxt
       in return (Assign var expr', (updateOneAV var (ATerm expr') ctxt), state)
    Assign var expr -> return (Assign var expr, (updateOneAV var (ATerm expr) ctxt), state) -- EBool, EInt, EFloat werden einfach so übernommen
    Send v -> return (setInContext' "send" ctxt v, ctxt, state)
    Receive v -> return (setInContext' "recv" ctxt v, ctxt, state)
    End v -> return (setInContext' "end" ctxt v, ctxt, state)
    _ -> return (st, ctxt, state)


-- used for "Send v"/"Receive v/End v" to trace
setInContext' :: String -> Context -> VarName -> Statement
setInContext' mode ctxt v =
  case lookupAV v ctxt of
    AIf cond (ATerm (EVar v1)) (AChan _) -> If cond (act mode v1) (act mode v)
    AIf cond (AChan _) (ATerm (EVar v1)) -> If cond (act mode v) (act mode v1)
    AIf cond (ATerm (EVar v1)) (ATerm (EVar v2)) ->
      if v1 == v2
        then act mode v1
        else If cond (act mode v1) (act mode v2)
    ATerm (EVar x) -- Es kann sehr wohl ein Channel im ATerm enthalten sein, wenn unser AV die Form ATerm (Evar x) hat und einem Channel zugewiesen ist
     -> act mode x
    -- Fallback
    _ -> act mode v
  where
    act :: String -> VarName -> Statement
    act m v =
      if m == "send"
        then Send v
        else if m == "recv"
               then Receive v
               else End v