module Datastructure where

import           Control.Monad       (replicateM)
import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.Map            as Map (Map, delete, deleteMin, empty,
                                             fromList, insert, lookup,
                                             lookupMin, map, mapWithKey, toList,
                                             union)
import           Data.SBV            (SBV, SBool, SInteger, SMTResult (..),
                                      SatResult (..), Symbolic, literal, runSMT,
                                      sBool, sDiv, sInteger, sMod, sNot, sTrue,
                                      sat, (.&&), (.<), (.<=), (.==), (.>),
                                      (.>=), (.||), free, ite)
import           Data.IORef

data Statement
  = Skip
  | Send VarName
  | Receive VarName
  | End VarName
  | Sequence Statement Statement
  | If Expr Statement Statement
  | For ForHeader Statement
  | Assign VarName Expr
  | Go Statement Statement
  | Declare VarName VarType
  | Make VarName ChanType
  deriving (Show, Eq)


-- new types for variable assignments and the list of variable declarations
newtype VarName =
  VarName String
  deriving (Eq, Ord)

newtype ChannelID =
  ChannelID String
  deriving (Show, Eq) -- type or newtype tbd

type FreshM = State Int -- State Int Monad for unique channelID naming

data ForHeader
  = ForHeaderRunning VarName Int Expr IncDec
  | ForHeaderRange VarName VarName
  deriving (Show, Eq)

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
  = AChan ChannelID -- Kanalname
  | AIf Expr AbstractVal AbstractVal -- eine Auswahl zwischen verschiedenen Abstract Values
  | ATerm Expr -- Ein Ausdruck
  | AUnknown -- noch unbekannt
  deriving (Show)

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

data Program =
  Program VarDecs Statement -- Parser output datatype of parseProgram

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

-- evaluates conditions and considers the context when there is a send/receive
stmtToST :: Context -> Int -> Statement -> IO (Either String (Statement, Context, Int))
stmtToST ctxt state st =
  case st of
    Declare v t -> do
      case typeCheck (Map.insert v (t, AUnknown) ctxt) of -- logic could added here to prevent redeclaring a variable that has already been declared
        Right _ -> return $ Right (Declare v t, (Map.insert v (t, AUnknown) ctxt), state)
        Left err -> return $ Left err
    Make v chant -> do
      let freshId = ChannelID ("oid" ++ show state)
      return (Right (Make v chant, updateOneAV v (AChan freshId) ctxt, state+1))
    Sequence s1 s2 -> do
      res1 <- stmtToST ctxt state s1
      case res1 of
        Right (s1', ctxt1, state1) -> do
          res2 <- stmtToST ctxt1 state1 s2
          case res2 of
            Right (s2', ctxt2, state2) -> return (Right (Sequence s1' s2', ctxt2, state2))
            Left err           -> return (Left err)
        Left err -> return (Left err)
    If e s1 s2 -> do
      res1 <- stmtToST ctxt state s1
      case res1 of
        Right (s1', ctxt1, state1) -> do
          res2 <- stmtToST ctxt state s2
          case res2 of
            Right (s2', ctxt2, state2) ->
              let e' = evalExpr e ctxt -- auch hier evalExpr, damit Variablen aus der Bedingung ihren aktuellen Wert aus dem AV im Context bekommen
                  ctxtMerged = mergeIfContexts e' ctxt1 ctxt2
               in case checkTypeOfExpression TBool e' ctxt of
                    True -> do
                      resolved <- resolveIf (If e' s1 s2) ctxt
                      if resolved == s1
                        then return (Right (s1', ctxt1, state1))
                        else if resolved == s2
                               then return (Right (s2', ctxt2, state2))
                               else if state1 > state2 then return (Right (If e' s1' s2', ctxtMerged, state1)) else return (Right (If e' s1' s2', ctxtMerged, state2))
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
          case typeCheck (updateOneAV var (ATerm e) ctxt) of
            Right () ->
              return (Right (Assign var e, updateOneAV var (ATerm e) ctxt, state)) -- var bekommt den Term der x definiert
            Left err -> return (Left err)
        AIf e av1 av2 ->
          case typeCheck (updateOneAV var (AIf e av1 av2) ctxt) of
            Right () ->
              return (Right (Assign var (EVar x), updateOneAV var (AIf e av1 av2) ctxt, state)) -- var bekommt die AIf condition von x
            Left err -> return (Left err)
        AChan chanID ->
          case typeCheck (updateOneAV var (AChan chanID) ctxt) of
            Right () ->
              return (Right (Assign var (EVar x), updateOneAV var (AChan chanID) ctxt, state))
        _ -> -- var bekommt einfach ATerm x
          case typeCheck (updateOneAV var (ATerm (EVar x)) ctxt) of
            Right _ ->
              return
                (Right
                   ( Assign var (EVar x)
                   , updateOneAV var (ATerm (EVar x)) ctxt, state))
    Assign var (EBinOp op e1 e2) ->
      let e1' = evalExpr e1 ctxt
          e2' = evalExpr e2 ctxt
       in case typeCheck (updateOneAV var (ATerm (EBinOp op e1' e2')) ctxt) of
            Right () ->
              return
                (Right
                   ( Assign var (EBinOp op e1' e2')
                   , updateOneAV var (ATerm (EBinOp op e1' e2')) ctxt, state))
            Left err -> return (Left err)
    Assign var (ENot expr) ->
      let expr' = evalExpr (ENot expr) ctxt
       in case typeCheck (updateOneAV var (ATerm expr') ctxt) of
            Right () ->
              return
                (Right (Assign var expr', updateOneAV var (ATerm expr') ctxt, state))
            Left err -> return (Left err)
    Assign var expr ->
      case typeCheck (updateOneAV var (ATerm expr) ctxt) of
        Right () ->
          return (Right (Assign var expr, updateOneAV var (ATerm expr) ctxt, state)) -- EBool, EInt, EFloat werden einfach so übernommen
        Left err -> return (Left err)
    Send v -> do
      case (setInContext "send" ctxt v) of
        Skip -> return (Left "Cannot send on non-existing channel")
        _ -> return (Right (setInContext "send" ctxt v, ctxt, state))
    Receive v -> do
      case (setInContext "recv" ctxt v) of 
        Skip -> return (Left "Cannot Receive on non-existing channel")
        _ -> return (Right (setInContext "recv" ctxt v, ctxt, state))
    End v -> 
      case (setInContext "end" ctxt v) of
        Skip -> return (Left "Cannot close non-existing channel")
        _ -> return (Right (setInContext "end" ctxt v, ctxt, state))
    Go s1 s2 -> do
      res <- stmtToST ctxt state s1
      case res of
        Left err -> return (Left err)
        Right (s1', ctxt1, state1) -> do
          res2 <- stmtToST ctxt1 state1 s2
          case res2 of
            Left err           -> return (Left err)
            Right (s2', ctxt2, state2) -> return (Right (Go s1' s2', ctxt2, state2))
    For hdr s -> do
      res <- stmtToST ctxt state s
      case res of
        Right (s', ctxt', state') -> return (Right (For hdr s', ctxt', state'))
        Left err          -> return (Left err)
    _ -> return (Right (st, ctxt, state))

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
                False ->
                  if e /= EVar (VarName "*")
                    then Left
                           $ "Type error: condition expression not of type Bool for variable "
                               ++ show var
                               ++ "."
                    else typeCheck (Map.fromList rest)
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
                False ->
                  if e /= EVar (VarName "*")
                    then Left
                           $ "Type error: condition expression not of type Bool for variable "
                               ++ show var
                               ++ "."
                    else typeCheck (Map.fromList rest)
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
                False ->
                  if e /= EVar (VarName "*")
                    then Left
                           $ "Type error: condition expression not of type Bool for variable "
                               ++ show var
                               ++ "."
                    else typeCheck (Map.fromList rest)
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
                False ->
                  if e /= EVar (VarName "*")
                    then Left
                           $ "Type error: condition expression not of type Bool for variable "
                               ++ show var
                               ++ "."
                    else typeCheck (Map.fromList rest)

checkTypeOfExpression :: VarType -> Expr -> Context -> Bool
checkTypeOfExpression expectedType expr ctxt =
  case expr of
    EVar (VarName "*") ->
      if expectedType == TBool
        then True
        else False
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
    Make (VarName ch) _ -> "make " ++ ch
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
      case onlyAssigns s of
        True -> ""
        _    -> block s ++ " if " ++ show e ++ " else " ++ "skip"
    If e Skip s ->
      case onlyAssigns s of
        True -> ""
        _    -> "skip" ++ " if " ++ show e ++ " else " ++ block s
    If e s1 s2
      | onlyAssigns s1 && onlyAssigns s2 -> ""
      | onlyAssigns s1 && not (onlyAssigns s2) ->
        "skip if " ++ show e ++ " else " ++ block s2
      | not (onlyAssigns s1) && onlyAssigns s2 ->
        block s1 ++ " if " ++ show e ++ " else skip"
      | otherwise -> block s1 ++ " if " ++ show e ++ " else " ++ block s2
    Go s1 s2 ->
      "go" ++ "{" ++ prettyPrintST s1 ++ "}" ++ "{" ++ prettyPrintST s2 ++ "}"
    Assign _ _ -> ""
    Declare _ _ -> ""
    For (ForHeaderRunning var start e incdec) s ->
      "for "
        ++ "("
        ++ show var
        ++ "="
        ++ show start
        ++ ";"
        ++ show e
        ++ ";"
        ++ show var
        ++ show incdec
        ++ ") "
        ++ block s
    For (ForHeaderRange var chan) s ->
      "for " ++ "(" ++ show var ++ " := range " ++ show chan ++ " " ++ show Skip
  where
    block :: Statement -> String
    block st@(Sequence _ _) = "{" ++ prettyPrintST st ++ "}"
    block st@(If _ _ _)     = "{" ++ prettyPrintST st ++ "}"
    block st                = prettyPrintST st

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

-- in the beginning all the Abstract Values are unknown
--initialContext :: Statement -> VarDecs -> Context
--initialContext stmt decs =
--  freshInitialContext
--    $ (Map.union
--         (extractContextFromStatement stmt Map.empty)
--         (Map.fromList [((x), (y, AUnknown)) | (x, y) <- decs]))
--
--extractContextFromStatement :: Statement -> Context -> Context
--extractContextFromStatement (Sequence s1 s2) ctxt =
--  extractContextFromStatement
--    s2
--    (Map.union (extractContextFromStatement s1 ctxt) ctxt)
--extractContextFromStatement _ ctxt = ctxt


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


-- returns variable that points to a specific channelID,
-- based on how VarNames with ChannelIDs are handled,
-- it should only be one variable at max, although it returns a list!
lookupVarNamesForChannel :: ChannelID -> Context -> [VarName]
lookupVarNamesForChannel ch ctxt =
  [var | (var, (_, AChan ch')) <- Map.toList ctxt, ch == ch']


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


-- flips the directions of all communications
dual :: Statement -> Statement
dual (Send var)       = Receive var
dual (Receive var)    = Send var
dual (Sequence s1 s2) = Sequence (dual s1) (dual s2)
dual (If e s1 s2)     = If e (dual s1) (dual s2)
dual (For head s)     = For head (dual s)
dual (Go s1 s2)       = Go (dual s1) (dual s2)
-- Assign, Skip End bleiben unverändert
dual x                = x

--data Statement
--  = Skip

--  | Declare VarName VarType
--  | Make VarName ChanType
--  deriving (Show, Eq)
-- go, for, new, some special if cases -> skip
strip :: Statement -> Statement
strip (Go s1 s2) = Skip
strip (For head s1) = Skip
strip (Assign var e) = Skip
strip (Make _ _) = Skip
strip (Declare _ _) = Skip
strip (Sequence s1 s2) = Sequence (strip s1) (strip s2)
strip (If e s1 s2) =
  case (strip s1, strip s2) of
    (Skip, Skip) -> Skip -- if ... then assign... else assign...
    (s1', s2')   -> If e s1' s2'
strip x = x

strip' :: Statement -> Statement
strip' (Go s1 s2) = Skip
strip' (For head s1) = Skip
strip' (Assign var e) = Skip
strip' (Sequence s1 s2) = Sequence (strip' s1) (strip' s2)
strip' (If e s1 s2) =
  case (strip' s1, strip' s2) of
    (Skip, Skip) -> Skip -- if ... then assign... else assign...
    (s1', s2')   -> If e s1' s2'
strip' x = x


-- (s1;s2);s3 ~ s1;(s2;s3)
-- s;skip ~ s
-- skip;s ~ s
assocIdRules :: Statement -> Statement
assocIdRules (Sequence (Skip) s)            = s
assocIdRules (Sequence s (Skip))            = s
assocIdRules (Sequence (Sequence s1 s2) s3) = Sequence s1 (Sequence s2 s3)
assocIdRules x                              = x


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
      assocIdRules (Sequence (applyFirstLevel s1) (applyFirstLevel s2))
    If e s1 s2 -> condEta (If e (applyFirstLevel s1) (applyFirstLevel s2))
    _ -> stmt


-- applies a rule until it doesn't change the input anymore
repeatApply :: (Statement -> Statement) -> Statement -> Statement
repeatApply f stmt =
  if (f stmt) == stmt
    then stmt
    else repeatApply f (f stmt)

phaseA :: Statement -> Statement
phaseA stmt = repeatApply applyFirstLevel stmt


-- if (e) {s1}{s2};s ~ if (e) {s1;s}{s2;s}
condDist :: Statement -> Statement
condDist (Sequence (If e s1 s2) s) = If e (Sequence s1 s) (Sequence s2 s)
condDist x                         = x


-- apply conddist once on all "nodes"
applySecondLevel :: Statement -> Statement
applySecondLevel stmt =
  case stmt of
    Sequence s1 s2 ->
      condDist (Sequence (applySecondLevel s1) (applySecondLevel s2))
    If e s1 s2 -> If e (applySecondLevel s1) (applySecondLevel s2)
    _ -> stmt

normalizeST :: Statement -> Statement
normalizeST stmt = normalize (strip stmt)
  where
    normalize stmt =
      let stmtA = phaseA stmt -- "Sie wenden die assoc, die id-Regeln und cond-eta so lange an, bis nichts mehr geht.
          stmtB = applySecondLevel stmtA
       in if (stmtA == stmtB) -- ändert condDist unseren ST?
            then stmtA -- Nein, dann keine Regel mehr anwendbar
            else normalize stmtB -- Ja, dann einmal cond-dist und wieder assoc, id, cond-eta usw bis keine dieser Regeln mehr anwendbar ist"

normalizeST' :: Statement -> Statement
normalizeST' stmt = normalize (strip' stmt)
  where
    normalize stmt = phaseA stmt

-- can test the equivalence of two normalized Statements!
testEquivalence :: Statement -> Statement -> Bool
testEquivalence s t =
  case (s, t) of
    (Send x, Send y) -> x == y -- atom-send
    (Receive x, Receive y) -> x == y -- atom-recv
    (End x, End y) -> x == y -- atom-end
  -- comp-rule s1 ~ s2 /\ s3 ~ s4 => s1; s3 ~ s2; s4
    (Sequence s1 s3, Sequence s2 s4) ->
      testEquivalence s1 s2 && testEquivalence s3 s4
  -- cond-rule s1 ~ s2 /\ s3 ~ s4 => if (e) {s1}{ s3} ~ if (e) {s2}{ s4}
    (If e s1 s3, If e' s2 s4) ->
      e == e' && testEquivalence s1 s2 && testEquivalence s3 s4
  -- else false
    _ -> False


-- generates the unique variable names
freshName :: FreshM VarName
freshName = do
  n <- get
  put (n + 1)
  return $ VarName ("a" ++ show n)


-- takes a normalized ST and renames all varnames for channels in the same way to
-- test if two STs are the same despite their different channel names
-- Map VarName VarName = alle neuen Zuweisungen von alt zu neu
canonicalizeChannelNames :: Statement -> Map.Map VarName VarName -> Statement
canonicalizeChannelNames stmt list =
  fst (evalState (canonicalizeHelper stmt list) 0)
  where
    canonicalizeHelper ::
         Statement
      -> Map.Map VarName VarName
      -> FreshM (Statement, Map.Map VarName VarName)
    canonicalizeHelper stmt assignments =
      case stmt of
        Send v ->
          case Map.lookup v assignments of
            Just new -> return ((Send new), assignments)
            Nothing -> do
              new <- freshName
              let assignments' = Map.insert v new assignments
              return ((Send new), assignments')
        Receive v ->
          case Map.lookup v assignments of
            Just new -> return ((Receive new), assignments)
            Nothing -> do
              new <- freshName
              let assignments' = Map.insert v new assignments
              return ((Receive new), assignments')
        End v ->
          case Map.lookup v assignments of
            Just new -> return ((End new), assignments)
            Nothing -> do
              new <- freshName
              let assignments' = Map.insert v new assignments
              return ((End new), assignments')
        If expr stmt1 stmt2 -> do
          (stmt1', assignments1) <- canonicalizeHelper stmt1 assignments
          (stmt2', assignments2) <- canonicalizeHelper stmt2 assignments1
          return ((If expr stmt1' stmt2'), assignments2)
        Sequence stmt1 stmt2 -> do
          (stmt1', assignments1) <- canonicalizeHelper stmt1 assignments
          (stmt2', assignments2) <- canonicalizeHelper stmt2 assignments1
          return ((Sequence stmt1' stmt2'), assignments2)
        x -> return (x, assignments)

hasVars :: Expr -> Bool
hasVars (EVar _)         = True
hasVars (EBinOp _ e1 e2) = hasVars e1 || hasVars e2
hasVars (ENot e)         = hasVars e
hasVars _                = False

--data AbstractVal
--  = AChan ChannelID -- Kanalname
--  | AIf Expr AbstractVal AbstractVal -- eine Auswahl zwischen verschiedenen Abstract Values
--  | ATerm Expr -- Ein Ausdruck
--  | AUnknown -- noch unbekannt
--  deriving (Show)
expressionToSymbolic :: SMTEnv -> Expr -> Context -> Symbolic SBVal
expressionToSymbolic env expr ctxt =
  case expr of
    EBool b -> return (SBVBool $ literal b) -- expression ist eine bool -> SBool
    EInt n -> return (SBVInt $ literal n) -- expression ist ein int -> SInteger
    EVar x -> -- expression ist eine Variable
      case lookupAV x ctxt of
        ATerm e | not (hasVars e) -> -- und diese Variable verweist NICHT auf andere Variablen
          expressionToSymbolic env e ctxt -- dann wird x ersetzt durch das, wofür x steht, also e
        --AIf cond (ATerm e1) (ATerm e2) -> do
        --  SBVBool c' <- expressionToSymbolic env cond ctxt
        --  v1 <- expressionToSymbolic env e1 ctxt
        --  v2 <- expressionToSymbolic env e2 ctxt
        --  case (v1, v2) of
        --    (SBVInt  x, SBVInt  y) -> pure (SBVInt  (ite c' x y))
        --    (SBVBool x, SBVBool y) -> pure (SBVBool (ite c' x y))
        --    _ -> error "AIf branches have different types"
        --    _ ->
        --      case Map.lookup x env of
        --        Just v  -> return v
        --        Nothing -> error ("SMTEnv missing var: " ++ show x)
        _ -> do
          case Map.lookup x env of 
            Just v -> return v 
            Nothing -> error ("SMTEnv missing var: " ++ show x)
--    EVar x -- expression ist eine Variable
--     ->
--      case lookupAV x ctxt of
--        ATerm e
--          | not (hasVars e) -- und diese Variable verweist NICHT auf andere Variablen
--           -> expressionToSymbolic e ctxt -- Dann wird einfach x ersetzt durch das, wofür x steht, also e
--        -- AIf e s1 s2 fehlt!!! TODO
--        _ -- und wenn diese Variable x doch auf min. eine andere variable verweist,
--         ->
--          case lookupType x ctxt of
--            -- dann wird einfach eine Symbolic Variable x erstellt, die entweder SInt oder SBool ist
--            Just TInt -> SBVInt <$> sInteger (show x)
--            Just TBool -> SBVBool <$> sBool (show x)
--            -- falls für x gar kein eintrag im kontext ist,
--            Nothing ->
--              if x == VarName "*"
--                then SBVBool <$> sBool "*"
--                else error "variable has no type" -- oder wir können keinen type für x finden
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
        Nothing ->
          if v == VarName "*"
            then do sv <- sBool "*"
                    return (v, SBVBool sv)
            else error ("variable has no type: " ++ show v)


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
      unsatNeg = isUnsat satNeg -- ist ~e unerfüllbar? Wenn ja dann then branch
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
resolveIf x ctxt = return x
