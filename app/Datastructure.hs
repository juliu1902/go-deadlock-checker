module Datastructure where

import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.Map            as Map (Map, delete, deleteMin,
                                             fromList, insert, lookup,
                                             lookupMin, map, toList, union, empty)
import           Data.SBV            (SBool, SInteger, SMTResult (..),
                                      SatResult (..), Symbolic, literal,
                                      sBool, sInteger, sMod, sNot, sTrue,
                                      sat, (.&&), (.<), (.<=), (.==), (.>),
                                      (.>=), (.||))

data Statement
  = Skip
  | Send VarName
  | Receive VarName
  | End VarName
  | Sequence Statement Statement
  | If Expr Statement Statement
  | Assign VarName Expr
  | Go Statement -- go {} {}
  | Declare VarName VarType
  | Make VarName ChanType Statement
  | Func  VarName VarDecs Statement -- func foo(...)
  | FuncCall VarName [VarName] -- foo(...)
  | GoCall VarName [VarName] -- go foo(...)
  deriving (Show, Eq)


-- new types for variable assignments and the list of variable declarations
newtype VarName =
  VarName String
  deriving (Eq, Ord)

newtype ChannelID =
  ChannelID String
  deriving (Show, Eq)

type FreshM = State Int -- State Int Monad for unique channelID naming

data IncDec
  = Inc
  | Dec
  deriving (Eq)

instance Show (IncDec)
 where
  show Inc = "++"
  show Dec = "--"

data ChanType
  = CInt
  | CBool
  deriving (Show, Eq)

data VarType
  = TInt
  | TBool
  | TFloat
  | TChan ChanType
  deriving (Show, Eq)

type VarDec = (VarName, VarType)

type VarDecs = [VarDec]

data AbstractVal
  = AChan ChannelID
  | AIf Expr AbstractVal AbstractVal
  | ATerm Expr
  | AUnknown
  deriving (Show, Eq)

data Expr
  = EVar VarName
  | EBool Bool
  | EInt Integer
  | EFloat Double
  | EBinOp BinOp Expr Expr
  | ENot Expr
  deriving (Eq)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Gt
  | Lt
  | Ge
  | Le
  | Eq
  | Neq
  | And
  | Or
  deriving (Eq)

type Context = Map.Map VarName (VarType, AbstractVal) -- datatype for the inferContext function which evaluates all abstract values

data Function =
  Function VarName VarDecs Statement -- Parser output datatype of parseFunction

data Functioncall =
  Functioncall VarName [VarName]


instance Show VarName
 where
  show (VarName x) = x

instance Show Expr where
  show e =
    case e of
      EVar x          -> show x
      EBool x         -> show x
      EInt x          -> show x
      EFloat x        -> show x
      EBinOp op e1 e2 -> show e1 ++ show op ++ show e2
      ENot x          -> "!" ++ show x

instance Show BinOp where
  show op =
    case op of
      Add -> "+"
      Sub -> "-"
      Mul -> "*"
      Div -> "/"
      Mod -> "%"
      Gt  -> ">"
      Lt  -> "<"
      Ge  -> ">="
      Le  -> "<="
      Eq  -> "=="
      Neq -> "!="
      And -> "&&"
      Or  -> "||"

data SBVal
  = SBVInt (SInteger)
  | SBVBool (SBool)
  deriving (Eq, Show)

type SMTEnv = Map.Map VarName SBVal

type FuncEnv = Map.Map VarName (VarDecs, Statement)

-- generates actual session type out of given Statement
stmtToST :: FuncEnv -> Context -> Int -> Statement -> IO (Either String (FuncEnv, Statement, Context, Int))
stmtToST funcs ctxt state st =
  case st of
    Declare v t -> do
      case typeCheck (Map.insert v (t, AUnknown) ctxt) of
        Right _ -> return $ Right (funcs, Declare v t, (Map.insert v (t, AUnknown) ctxt), state)
        Left err -> return $ Left err
    Make v chant stmt -> do
      let idname = "oid" ++ show state
      let freshId = ChannelID idname
      let ctxt' = updateOneAV v (AChan freshId) ctxt
      res <- stmtToST funcs ctxt' (state+1) stmt
      case res of
        Right (f, stmt', ctxt'', state') -> return (Right (f, Make (VarName idname) chant stmt', ctxt'', state'))
        Left err -> return (Left err)
    Sequence s1 s2 -> do
      res1 <- stmtToST funcs ctxt state s1
      case res1 of
        Right (f, s1', ctxt1, state1) -> do
          res2 <- stmtToST f ctxt1 state1 s2
          case res2 of
            Right (f2, s2', ctxt2, state2) -> return (Right (f2, Sequence s1' s2', ctxt2, state2))
            Left err           -> return (Left err)
        Left err -> return (Left err)
    If e s1 s2 -> do
      res1 <- stmtToST funcs ctxt state s1
      case res1 of
        Right (f1, s1', ctxt1, state1) -> do
          res2 <- stmtToST funcs ctxt state s2
          case res2 of
            Right (f2, s2', ctxt2, state2) ->
              let e' = evalExpr e ctxt
                  ctxtMerged = mergeIfContexts e' ctxt1 ctxt2
               in case checkTypeOfExpression TBool e' ctxt of
                    True -> do
                      resolved <- resolveIf (If e' s1 s2) ctxt
                      if resolved == s1
                        then return (Right (f1, s1', ctxt1, state1))
                        else if resolved == s2
                               then return (Right (f2, s2', ctxt2, state2))
                               else if state1 > state2 then return (Right (Map.union f1 f2, If e' s1' s2', ctxtMerged, state1)) else return (Right (Map.union f1 f2, If e' s1' s2', ctxtMerged, state2))
                    False ->
                      return
                        (Left
                           $ "Type error: condition expression not of type Bool in If statement.")
            Left err -> return (Left err)
        Left err -> return (Left err)
    Assign var (EVar x) -- var = x
     ->
      case lookupAV x ctxt of
        ATerm e ->
          withTypeUpdate var (ATerm e) funcs (Assign var e) ctxt state
        AIf e av1 av2 ->
          withTypeUpdate var (AIf e av1 av2) funcs (Assign var (EVar x)) ctxt state
        AChan chanID ->
          withTypeUpdate var (AChan chanID) funcs (Assign var (EVar x))  ctxt state
        _ -> -- var just gets ATerm x
          withTypeUpdate var (ATerm (EVar x)) funcs (Assign var (EVar x)) ctxt state
    Assign var (EBinOp op e1 e2) -> do
      let e1' = evalExpr e1 ctxt
          e2' = evalExpr e2 ctxt
          av  = ATerm (EBinOp op e1' e2')
      withTypeUpdate var av funcs (Assign var (EBinOp op e1' e2')) ctxt state
    Assign var (ENot e) -> do
      let e' = evalExpr (ENot e) ctxt
          av = ATerm e'
      withTypeUpdate var av funcs (Assign var e') ctxt state
    Assign var expr -> do
      let e' = evalExpr expr ctxt
          av = ATerm e'
      withTypeUpdate var av funcs (Assign var e') ctxt state
    Send v -> do
      case (setInContext "send" ctxt v) of
        Skip -> return (Left "Cannot send on non-existing channel")
        _ -> return (Right (funcs, setInContext "send" ctxt v, ctxt, state))
    Receive v -> do
      case (setInContext "recv" ctxt v) of
        Skip -> return (Left "Cannot Receive on non-existing channel")
        _ -> return (Right (funcs, setInContext "recv" ctxt v, ctxt, state))
    End v ->
      case (setInContext "end" ctxt v) of
        Skip -> return (Left "Cannot close non-existing channel")
        _ -> return (Right (funcs, setInContext "end" ctxt v, ctxt, state))
    Func x decs stmt -> do
      st' <- stmtToST Map.empty (initialContext decs) 0 stmt
      case st' of
        Left err -> return (Left err)
        Right (f', st'', _, _) -> return (Right (Map.insert x (decs, stmt) funcs, Skip, ctxt, state))
    GoCall name args -> do
      case Map.lookup name funcs of
        Nothing -> return (Left ("Unknown function in go call: " ++ show name))
        Just (params, stmt) -> do
          let projections = [(p, a) | ((p, _), a) <- zip params args]
              substituted = replaceAll projections stmt
          res <- stmtToST funcs ctxt state substituted
          case res of
            Left err -> return (Left err)
            Right (_, goroutineST, _, state') ->
              return (Right (funcs, Go goroutineST, ctxt, state'))
    FuncCall name args -> do
      case Map.lookup name funcs of
        Nothing -> return (Left ("Unknown function in go call: " ++ show name))
        Just (params, stmt) -> do
          let projections = [(p, a) | ((p, _), a) <- zip params args]
              substituted = replaceAll projections stmt
          res <- stmtToST funcs ctxt state substituted
          case res of
            Left err -> return (Left err)
            Right (_, funcST, _, state') ->
              return (Right (funcs, funcST, ctxt, state'))
    _ -> return (Right (funcs, st, ctxt, state))

replaceAll :: [(VarName, VarName)] -> Statement -> Statement
replaceAll ((old, new):xs) stmt = replaceAll xs (replace (old,new) stmt)
replaceAll [] stmt = stmt

replace :: (VarName, VarName) -> Statement -> Statement
replace mapping@(old, new) stmt = case stmt of
  Skip -> Skip
  Sequence st1 st2 -> Sequence (replace mapping st1) (replace mapping st2)
  Send v -> if v==old then Send new else Send v
  Receive v -> if v==old then Receive new else Receive v
  End v -> if v==old then End new else End v
  If e s1 s2 -> If (replaceExpression mapping e) (replace mapping s1) (replace mapping s2)
  Assign var expr -> if var==old then Assign new (replaceExpression mapping expr) else Assign old (replaceExpression mapping expr)
  Make var t st -> if var==old then Make new t (replace mapping st) else Make var t (replace mapping st)
  Go st -> Go $ replace mapping st
  x -> x

replaceExpression :: (VarName, VarName) -> Expr -> Expr
replaceExpression (old, new) e = case e of
  EVar x -> if x==old then EVar new else EVar x
  EBinOp op e1 e2 -> EBinOp op (replaceExpression (old, new) e1) (replaceExpression (old,new) e2)
  ENot expr -> ENot (replaceExpression (old, new) expr)
  x -> x

addSkip :: Statement -> Statement
addSkip stmt = case stmt of
  Sequence _ Skip -> stmt
  Sequence s1 s2 -> Sequence s1 (addSkip s2)
  Make x v st -> Make x v (addSkip st)
  Skip -> stmt
  x -> Sequence x Skip

withTypeUpdate :: VarName -> AbstractVal -> FuncEnv -> Statement -> Context -> Int -> IO (Either String (FuncEnv, Statement, Context, Int))
withTypeUpdate var av funcs stOut ctxt state =
  let ctxt' = updateOneAV var av ctxt
  in case typeCheck ctxt' of
       Right () -> return (Right (funcs, stOut, ctxt', state))
       Left err -> return (Left err)

typeCheck :: Context -> Either String ()
typeCheck ctxt =
  case Map.toList ctxt of
    [] -> Right ()
    ((var, (vtype, aval)):rest) ->
      case vtype of
        TInt ->
          case aval of
            ATerm (EInt x) -> typeCheck ((Map.fromList rest))
            ATerm (EVar var) ->
              case Map.lookup var ctxt of
                Just (TInt, _) -> typeCheck (Map.fromList rest)
                _ ->
                  Left
                    $ "Type error: variable " ++ show var ++ " not of type Int."
            ATerm (EBinOp op e1 e2) ->
              case op of
                Gt -> Left "Gt operator not allowed for Int"
                Lt -> Left "Lt operator not allowed for Int"
                Ge -> Left "Ge operator not allowed for Int"
                Le -> Left "Le operator not allowed for Int"
                Eq -> Left "Eq operator not allowed for Int"
                Neq -> Left "Neq operator not allowed for Int"
                And -> Left "And operator not allowed for Int"
                Or -> Left "Or operator not allowed for Int"
                _ ->
                  case checkTypeOfExpression TInt e1 ctxt
                         && checkTypeOfExpression TInt e2 ctxt of
                    True -> typeCheck (Map.fromList rest)
                    False ->
                      Left
                        $ "Type error in binary operation on variable "
                            ++ show var
                            ++ "."
            ATerm (ENot expr) ->
              Left
                $ "Type error: "
                    ++ show expr
                    ++ " cannot be negated as variable is of type Int."
            ATerm _ ->
              Left $ "Type error: variable " ++ show var ++ " not of type Int."
            AUnknown -> typeCheck (Map.fromList rest)
            AIf e av1 av2 ->
              case checkTypeOfExpression TBool e ctxt of
                True -> typeCheck (Map.fromList rest)
                False -> Left
                           $ "Type error: condition expression not of type Bool for variable "
                               ++ show var
                               ++ "."
            AChan id ->
              Left
                $ "Type error: variable "
                    ++ show var
                    ++ " not of type Int (Cannot assign Channel to Int)."
        TBool ->
          case aval of
            ATerm (EBool x) -> typeCheck (Map.fromList rest)
            ATerm (EVar var) ->
              case Map.lookup var ctxt of
                Just (TBool, _) -> typeCheck (Map.fromList rest)
                _ ->
                  Left
                    $ "Type error: variable "
                        ++ show var
                        ++ " not of type Bool."
            ATerm (EBinOp op e1 e2) ->
              case op of
                And ->
                  case checkTypeOfExpression TBool e1 ctxt
                         && checkTypeOfExpression TBool e2 ctxt of
                    True -> typeCheck (Map.fromList rest)
                    False ->
                      Left
                        $ "Type error in binary operation on variable "
                            ++ show var
                            ++ "."
                Or ->
                  case checkTypeOfExpression TBool e1 ctxt
                         && checkTypeOfExpression TBool e2 ctxt of
                    True -> typeCheck (Map.fromList rest)
                    False ->
                      Left
                        $ "Type error in binary operation on variable "
                            ++ show var
                            ++ "."
                Add -> Left "Add operator not allowed for Bool"
                Sub -> Left "Sub operator not allowed for Bool"
                Mul -> Left "Mul operator not allowed for Bool"
                Div -> Left "Div operator not allowed for Bool"
                Mod -> Left "Mod operator not allowed for Bool"
                _ ->
                  case ( checkTypeOfExpression TInt e1 ctxt
                       , checkTypeOfExpression TInt e2 ctxt) of
                    (True, True) -> typeCheck (Map.fromList rest)
                    _            -> Left "no int detected in comparison"
            ATerm (ENot expr) ->
              case checkTypeOfExpression TBool expr ctxt of
                True -> typeCheck (Map.fromList rest)
                False ->
                  Left
                    $ "Type error: "
                        ++ show expr
                        ++ " not of type bool and cannot be negated."
            ATerm _ ->
              Left $ "Type error: variable " ++ show var ++ " not of type Bool."
            AUnknown -> typeCheck (Map.fromList rest)
            AIf e av1 av2 ->
              case checkTypeOfExpression TBool e ctxt of
                True -> typeCheck (Map.fromList rest)
                False -> Left
                           $ "Type error: condition expression not of type Bool for variable "
                               ++ show var
                               ++ "."
            AChan id ->
              Left
                $ "Type error: variable "
                    ++ show var
                    ++ " not of type Bool (Cannot assign Channel to Bool)."
        TChan CInt ->
          case aval of
            AChan id -> typeCheck (Map.fromList rest)
            ATerm (EVar var) ->
              case Map.lookup var ctxt of
                Just (TChan CInt, _) -> typeCheck (Map.fromList rest)
                _ ->
                  Left
                    $ "Type error: variable "
                        ++ show var
                        ++ " not of type chan int."
            ATerm (EBinOp op e1 e2) ->
              Left
                $ "Cannot operate binary operation on channel variable"
                    ++ show var
                    ++ "."
            ATerm (ENot expr) ->
              Left $ "Cannot negate a channel variable of type int"
            ATerm _ ->
              Left
                $ "Type error: variable "
                    ++ show var
                    ++ " not of type chan int."
            AUnknown -> typeCheck (Map.fromList rest)
            AIf e av1 av2 ->
              case checkTypeOfExpression TBool e ctxt of
                True -> typeCheck (Map.fromList rest)
                False -> Left
                           $ "Type error: condition expression not of type Bool for variable "
                               ++ show var
                               ++ "."
        TChan CBool ->
          case aval of
            AChan id -> typeCheck (Map.fromList rest)
            ATerm (EVar var) ->
              case Map.lookup var ctxt of
                Just (TChan CBool, _) -> typeCheck (Map.fromList rest)
                _ ->
                  Left
                    $ "Type error: variable "
                        ++ show var
                        ++ " not of type chan bool."
            ATerm (EBinOp op e1 e2) ->
              case checkTypeOfExpression (TChan CBool) e1 ctxt
                     && checkTypeOfExpression (TChan CBool) e2 ctxt of
                True -> typeCheck (Map.fromList rest)
                False ->
                  Left
                    $ "Type error in binary operation on variable "
                        ++ show var
                        ++ "."
            ATerm (ENot expr) ->
              Left
                $ "Type error: cannot negate a channel variable of type bool."
            ATerm _ ->
              Left
                $ "Type error: variable "
                    ++ show var
                    ++ " not of type chan bool."
            AUnknown -> typeCheck (Map.fromList rest)
            AIf e av1 av2 ->
              case checkTypeOfExpression TBool e ctxt of
                True -> typeCheck (Map.fromList rest)
                False -> Left
                           $ "Type error: condition expression not of type Bool for variable "
                               ++ show var
                               ++ "."

checkTypeOfExpression :: VarType -> Expr -> Context -> Bool
checkTypeOfExpression expectedType expr ctxt =
  case expr of
    EVar var ->
      case Map.lookup var ctxt of
        Just (vtype, _) -> vtype == expectedType
        Nothing         -> False
    EBool x -> expectedType == TBool
    EInt x -> expectedType == TInt
    EFloat x -> expectedType == TFloat
    ENot e1 -> expectedType == TBool && checkTypeOfExpression TBool e1 ctxt
    EBinOp op e1 e2 ->
      case op of
        Add ->
          if expectedType == TInt
            then checkTypeOfExpression expectedType e1 ctxt
                   && checkTypeOfExpression expectedType e2 ctxt
            else False
        Sub ->
          if expectedType == TInt
            then checkTypeOfExpression expectedType e1 ctxt
                   && checkTypeOfExpression expectedType e2 ctxt
            else False
        Mul ->
          if expectedType == TInt
            then checkTypeOfExpression expectedType e1 ctxt
                   && checkTypeOfExpression expectedType e2 ctxt
            else False
        Div ->
          if expectedType == TInt
            then checkTypeOfExpression expectedType e1 ctxt
                   && checkTypeOfExpression expectedType e2 ctxt
            else False
        Mod ->
          if expectedType == TInt
            then checkTypeOfExpression expectedType e1 ctxt
                   && checkTypeOfExpression expectedType e2 ctxt
            else False
        And ->
          if expectedType == TBool
            then checkTypeOfExpression TBool e1 ctxt
                   && checkTypeOfExpression TBool e2 ctxt
            else False
        Or ->
          if expectedType == TBool
            then checkTypeOfExpression TBool e1 ctxt
                   && checkTypeOfExpression TBool e2 ctxt
            else False
        _ ->
          if expectedType == TBool
            then checkTypeOfExpression TInt e1 ctxt
                   && checkTypeOfExpression TInt e2 ctxt
                   || checkTypeOfExpression TFloat e1 ctxt
                        && checkTypeOfExpression TFloat e2 ctxt
                   || checkTypeOfExpression TBool e1 ctxt
                        && checkTypeOfExpression TBool e2 ctxt
            else False


-- evaluates an expression based on the current context
-- only really relevant for expressions that are variables
evalExpr :: Expr -> Context -> Expr
evalExpr (EVar x) ctxt =
  case lookupAV x ctxt of
    ATerm e -> e -- wenn unsere expr eine Variable ist und durch einen Term definiert, dann gebe diese Term zurück
    _       -> EVar x -- ansonsten gebe einfach diese Variable zurück
evalExpr (EBinOp op e1 e2) ctxt =
  let e1' = evalExpr e1 ctxt
      e2' = evalExpr e2 ctxt
   in EBinOp op e1' e2'
evalExpr (ENot e1) ctxt =
  case evalExpr e1 ctxt of
    EBool True  -> EBool False
    EBool False -> EBool True
    e1'         -> ENot e1'
evalExpr e _ = e

setInContext :: String -> Context -> VarName -> Statement
setInContext mode ctxt v =
  case lookupAV v ctxt of
    AChan (ChannelID chanID) ->
      act mode (VarName chanID)

    AIf cond av1 av2 ->
      If cond (stmtForAV av1) (stmtForAV av2)
    _ -> Skip -- Skip indicates there has been an error
  where
    act m x
      | m == "send" = Send x
      | m == "recv" = Receive x
      | otherwise   = End x

    stmtForAV av =
      case av of
        AChan (ChannelID cid) -> act mode (VarName cid)
        AIf c a b             -> If c (stmtForAV a) (stmtForAV b)
        _                     -> act mode v  -- fallback


-- representing a parsed Statement as a Session Type, optionally used after stmtToST!
prettyPrintST :: Statement -> String
prettyPrintST x =
  case x of
    Make (VarName ch) _ stmt -> "make " ++ ch ++ "." ++ prettyPrintST stmt
    Skip -> "skip"
    Send (VarName ch) -> ch ++ "!"
    Receive (VarName ch) -> ch ++ "?"
    End (VarName ch) -> ch ++ "#"
    Sequence s1 s2 ->
      let a = prettyPrintST s1
          b = prettyPrintST s2
       in case (null a, null b) of
            (True, True)   -> ""
            (True, False)  -> b
            (False, True)  -> a
            (False, False) -> a ++ ";" ++ b
        -- assignments werden nicht mit ; getrennt sondern ignoriert
    If _ (Assign _ _) (Assign _ _) -> "" -- ifs mit nur assigns werden ignoriert
    If e s Skip ->
      (if onlyAssigns s then "" else "if " ++ show e ++ " " ++ block s ++ "{skip}")
    If e Skip s ->
      (if onlyAssigns s then "" else "if " ++ show e ++ " {skip}" ++ block s)
    If e s1 s2
      | onlyAssigns s1 && onlyAssigns s2 -> ""
      | onlyAssigns s1 && not (onlyAssigns s2) ->
        "if " ++ show e ++ " {skip}" ++ block s2
      | not (onlyAssigns s1) && onlyAssigns s2 ->
        "if " ++ show e ++ " " ++ block s1 ++ "{skip}"
      | otherwise -> "if " ++ show e ++ " " ++ block s1 ++ block s2
    Go s1 ->
      "go" ++ "(" ++ prettyPrintST s1 ++ ")"
    Assign _ _ -> ""
    Declare _ _ -> ""
    Func name vars p -> "func " ++ show name ++ " " ++ show vars ++ "{" ++ prettyPrintST p ++ "}"
    GoCall _ _ -> ""
    FuncCall _ _ -> ""
  where
    block :: Statement -> String
    block st@(Sequence _ _) = "{" ++ prettyPrintST st ++ "}"
    block st@(If _ _ _)     = "{" ++ prettyPrintST st ++ "}"
    block st                = "{" ++ prettyPrintST st ++ "}"

onlyAssigns :: Statement -> Bool
onlyAssigns (Sequence s1 s2) = (onlyAssigns s1) && (onlyAssigns s2)
onlyAssigns (Assign _ _)     = True
onlyAssigns Skip             = True -- Skip verändert nichts
onlyAssigns _                = False


-- generates the unique channelID names
freshChannel :: FreshM ChannelID
freshChannel = do
  n <- get
  put (n + 1)
  return $ ChannelID ("id" ++ show n)

initialContext :: VarDecs -> Context
initialContext decs =
  freshInitialContext (Map.fromList [((x), (y, AUnknown)) | (x, y) <- decs])

-- var chan int/bool should automatically create fresh channels with unique channelID
freshInitialContext :: Context -> Context
freshInitialContext initialc = evalState (traverse freshOne initialc) 0
  where
    freshOne :: (VarType, AbstractVal) -> FreshM (VarType, AbstractVal)
    freshOne (t, av) = do
      case t of
        TChan x -> do
          newid <- freshChannel
          return (t, AChan newid)
        _ -> return (t, AUnknown)


-- finds vartype of a variable
lookupType :: VarName -> Context -> Maybe VarType
lookupType x ctxt = fmap fst (Map.lookup x ctxt)


-- finds AV of a variable
lookupAV :: VarName -> Context -> AbstractVal
lookupAV x ctxt = maybe AUnknown snd (Map.lookup x ctxt)

-- writes one AbstractVal-update in current ctxt
updateOneAV :: VarName -> AbstractVal -> Context -> Context
updateOneAV x av ctxt =
  case Map.lookup x ctxt of
    Just (t, _) -> Map.insert x (t, av) $ ctxt -- Keep existing context by using $ instead of direct application
    _           -> ctxt


-- the analysed "if-then-else branch" brought two contexts
-- mergeIfContexts merges them into one
mergeIfContexts :: Expr -> Context -> Context -> Context
mergeIfContexts cond c1 c2 =
  case Map.lookupMin c1 of
    Nothing -> Map.map (\(t, av2) -> (t, AIf cond AUnknown av2)) c2 -- values in c2 that have not been analyzed yet are considered!
    Just (k, (t, av)) ->
      case Map.lookup k c2 of
        Nothing ->
          Map.insert
            k
            (t, AIf cond av AUnknown)
            (mergeIfContexts cond (Map.deleteMin c1) c2)
        Just (t2, av2) ->
          if abstractEq av av2
            then Map.insert
                   k
                   (t, av)
                   (mergeIfContexts cond (Map.deleteMin c1) (Map.delete k c2))
            else Map.insert
                   k
                   (t, AIf cond av av2)
                   (mergeIfContexts cond (Map.deleteMin c1) (Map.delete k c2)) -- it's required for abs and abs2 to have the same VarType!
      where abstractEq :: AbstractVal -> AbstractVal -> Bool
            abstractEq (AChan c1) (AChan c2) = c1 == c2
            abstractEq (ATerm e1) (ATerm e2) = e1 == e2
            abstractEq (AIf e a b) (AIf e' a' b') = e == e' && abstractEq a a' && abstractEq b b'
            abstractEq AUnknown AUnknown     = True
            abstractEq _ _                   = False

strip' :: Statement -> Statement
strip' (Assign _ _) = Skip
strip' (Declare _ _) = Skip
strip' (Make v t stmt) = Make v t (strip' stmt)
strip' (Sequence s1 s2) = Sequence (strip' s1) (strip' s2)
strip' (If e s1 s2) =
  case (strip' s1, strip' s2) of
    (Skip, Skip) -> Skip -- if ... then assign... else assign...
    (s1', s2')   -> If e s1' s2'
strip' x = x

-- (s1;s2);s3 ~ s1;(s2;s3)
-- skip;s ~ s
assocIdRule :: Statement -> Statement
assocIdRule (Sequence Skip s) = s
assocIdRule (Sequence (Sequence s1 s2) s3) = Sequence s1 (Sequence s2 s3)
assocIdRule x = x

-- idFor not necessary, becomes skip by calling strip function
-- cond-eta: if (e) {s}{s} ~ s
condEta :: Statement -> Statement
condEta (If e s1 s2) =
  if s1 == s2
    then s1
    else (If e s1 s2)
condEta x = x

-- assoc, id, condeta is applied once on all "nodes"
applyFirstLevel :: Statement -> Statement
applyFirstLevel stmt =
  case stmt of
    Sequence s1 s2 ->
      assocIdRule (Sequence (applyFirstLevel s1) (applyFirstLevel s2))
    If e s1 s2 -> condEta (If e (applyFirstLevel s1) (applyFirstLevel s2))
    _ -> stmt

simplification :: Statement -> Statement
simplification stmt = repeatApplication applyAllRules (strip' stmt)
  where
    repeatApplication :: (Statement -> Statement) -> Statement -> Statement
    repeatApplication f s =
      if f s == s
        then s
        else repeatApplication f (f s)
    applyAllRules :: Statement -> Statement
    applyAllRules s = case s of
      Sequence s1 s2 -> assocIdRule (Sequence (applyAllRules s1) (applyAllRules s2))
      If e s1 s2 -> condEta (If e (applyFirstLevel s1) (applyFirstLevel s2))
      _ -> s

-- generates the unique variable names
freshName :: FreshM VarName
freshName = do
  n <- get
  put (n + 1)
  return $ VarName ("a" ++ show n)

hasVars :: Expr -> Bool
hasVars (EVar _)         = True
hasVars (EBinOp _ e1 e2) = hasVars e1 || hasVars e2
hasVars (ENot e)         = hasVars e
hasVars _                = False

expressionToSymbolic :: SMTEnv -> Expr -> Context -> Symbolic SBVal
expressionToSymbolic env expr ctxt =
  case expr of
    EBool b -> return (SBVBool $ literal b) -- expression ist eine bool -> SBool
    EInt n -> return (SBVInt $ literal n) -- expression ist ein int -> SInteger
    EVar x -> -- expression ist eine Variable
      case lookupAV x ctxt of
        ATerm e | not (hasVars e) -> -- und diese Variable verweist NICHT auf andere Variablen
          expressionToSymbolic env e ctxt -- dann wird x ersetzt durch das, wofür x steht, also e
        _ -> do
          case Map.lookup x env of
            Just v -> return v
            Nothing -> error ("SMTEnv missing var: " ++ show x)
    ENot e -> do
      val <- expressionToSymbolic env e ctxt
      case val of
        SBVBool b -> return (SBVBool (sNot b))
        _         -> error "negating non-bool expression"
    EBinOp op e1 e2 -> do
      first <- expressionToSymbolic env e1 ctxt
      second <- expressionToSymbolic env e2 ctxt
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

varsInExpr :: Expr -> [VarName]
varsInExpr e = List.nub (go e)
  where
    go (EVar v)         = [v]
    go (EBinOp _ a b)   = go a ++ go b
    go (ENot a)         = go a
    go _                = []

mkEnv :: Expr -> Context -> Symbolic SMTEnv
mkEnv e ctxt = do
  let vs = varsInExpr e
  pairs <- mapM mkOne vs
  return (Map.fromList pairs)
  where
    mkOne v =
      case lookupType v ctxt of
        Just TInt  -> do sv <- sInteger (show v)
                         return (v, SBVInt sv)
        Just TBool -> do sv <- sBool (show v)
                         return (v, SBVBool sv)
        Nothing -> error ("variable has no type: " ++ show v)


-- converts SatResult of `checkSat` to a Bool and reformulate the satisfiability question to: "is x unsatisfiable?"
isUnsat :: SatResult -> Bool
isUnsat (SatResult r) =
  case r of
    Unsatisfiable _ _ -> True
    _                 -> False

-- takes an If condition and the current context and returns whether this condition is satisfiable or unsatisfiable
checkSat :: Expr -> Context -> IO SatResult
checkSat expr ctxt =
  sat $ do
    env <- mkEnv expr ctxt
    val <- expressionToSymbolic env expr ctxt
    case val of
      SBVBool b -> return b
      _         -> return sTrue

checkIfBranches :: Expr -> Context -> IO String
checkIfBranches cond ctxt = do
  satCond <- checkSat cond ctxt
  satNeg <- checkSat (ENot cond) ctxt
  let unsatCond = isUnsat satCond -- ist e aus 'if e...' unerfüllbar? Wenn ja dann else branch
      unsatNeg = isUnsat satNeg -- ist \neg e unerfüllbar? Wenn ja dann then branch
  case (unsatCond, unsatNeg) of
    (True, False)  -> return "else branch"
    (False, True)  -> return "then branch"
    (False, False) -> return "case split"
    (_, _)         -> return "If Branch komplett ignorieren"

resolveIf :: Statement -> Context -> IO Statement
resolveIf (If e s1 s2) ctxt = do
  res <- checkIfBranches e ctxt
  case res of
    "else branch" -> return s2
    "then branch" -> return s1
    "case split"  -> return (If e s1 s2)
    _             -> return Skip
resolveIf x _ = return x
