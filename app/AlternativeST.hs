module AlternativeST where

import qualified Data.Map      as Map (Map, delete, deleteMin, empty, fromList,
                                       insert, lookup, lookupMin, map,
                                       mapWithKey, toList, union)
import           Datastructure

-- evaluates conditions and considers the context when there is a send/receive
alternativeSTNaming :: Context -> Statement -> IO (Statement, Context)
alternativeSTNaming ctxt st =
  case st of
    Declare v t -> do
      case t of
        t@(TChan _) -> return (Declare v t, ctxt)
        _           -> return (Declare v t, (Map.insert v (t, AUnknown) ctxt))
    Sequence s1 s2 -> do
      (s1', ctxt1) <- alternativeSTNaming ctxt s1
      (s2', ctxt2) <- alternativeSTNaming ctxt1 s2
      return (Sequence s1' s2', ctxt2)
    If e s1 s2 -> do
      (s1', ctxt1) <- alternativeSTNaming ctxt s1
      (s2', ctxt2) <-  alternativeSTNaming ctxt s2
      res1 <- resolveIf (If (evalExpr e ctxt) s1 s2) ctxt
      let e' = evalExpr e ctxt
          ctxtMerged = (mergeIfContexts e' ctxt1 ctxt2)
       in if res1 == s1 then return (s1', ctxt1) else if res1 == s2 then return (s2', ctxt2) else return (If e' s1' s2', ctxtMerged)
    Assign var (EVar x) ->
      case lookupAV x ctxt of
        ATerm e -> return (Assign var e, (updateOneAV var (ATerm e) ctxt)) -- var bekommt den Term der x definiert
        AIf e av1 av2 ->
          return (Assign var (EVar x), (updateOneAV var (AIf e av1 av2) ctxt)) -- var bekommt die AIf condition von x
        _ -> return (Assign var (EVar x), (updateOneAV var (ATerm (EVar x)) ctxt))
    Assign var (EBinOp op e1 e2) ->
      let e1' = evalExpr e1 ctxt
          e2' = evalExpr e2 ctxt
       in return ( Assign var (EBinOp op e1' e2')
          , (updateOneAV var (ATerm (EBinOp op e1' e2')) ctxt))
    Assign var (ENot expr) ->
      let expr' = evalExpr (ENot expr) ctxt
       in return (Assign var expr', (updateOneAV var (ATerm expr') ctxt))
    Assign var expr -> return (Assign var expr, (updateOneAV var (ATerm expr) ctxt)) -- EBool, EInt, EFloat werden einfach so übernommen
    Send v -> return (setInContext' "send" ctxt v, ctxt)
    Receive v -> return (setInContext' "recv" ctxt v, ctxt)
    End v -> return (setInContext' "end" ctxt v, ctxt)
    _ -> return (st, ctxt)


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