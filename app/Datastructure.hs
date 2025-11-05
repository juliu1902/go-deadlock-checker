module Datastructure where

import           Control.Monad       (replicateM)
import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.Map            as Map (Map, delete, deleteMin, empty,
                                             fromList, insert, lookup,
                                             lookupMin, map, toList)

data Statement
  = New VarName Statement
  | Skip
  | Send VarName
  | Receive VarName
  | End VarName
  | Sequence Statement Statement
  | If Expr Statement Statement
  | For ForHeader Statement
  | Assign VarName Expr
  | Go Statement Statement
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
  | ATerm Expr -- Ein Ausdruck, der definitiv keinen Kanal enthält
  | AUnknown -- noch unbekannt
  deriving (Show)

data Expr
  = EVar VarName
  | EBool Bool
  | EInt Integer
  | EFloat Double
  | EBinOp BinOp Expr Expr
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


-- evaluates conditions and considers the context when there is a send/receive
stmtToST :: Context -> Statement -> (Statement, Context)
stmtToST ctxt st =
  case st of
    New c s ->
      let (s', ctxt') = stmtToST ctxt s
       in (New c s', ctxt')
    Sequence s1 s2 ->
      let (s1', ctxt1) = stmtToST ctxt s1
          (s2', ctxt2) = stmtToST ctxt1 s2
       in (Sequence s1' s2', ctxt2)
    If e s1 s2 ->
      let e' = evalExpr e ctxt -- auch hier evalExpr, damit Variablen aus der Bedingung ihren aktuellen Wert aus dem AV im Context bekommen
          (s1', ctxt1) = stmtToST ctxt s1
          (s2', ctxt2) = stmtToST ctxt s2
          ctxtMerged = mergeIfContexts e' ctxt1 ctxt2
       in (If e' s1' s2', ctxtMerged)
    -- var = x
    Assign var (EVar x) ->
      case lookupAV x ctxt of
        ATerm e -> (Assign var e, updateOneAV var (ATerm e) ctxt) -- var bekommt den Term der x definiert
        AIf e av1 av2 ->
          (Assign var (EVar x), updateOneAV var (AIf e av1 av2) ctxt) -- var bekommt die AIf condition von x
        _ -> (Assign var (EVar x), updateOneAV var (ATerm (EVar x)) ctxt) -- ACHTUNG hier könnte getestet werden, ob var zb ein Channel ist und EVar x auch und wenn sie sich im typen unterscheiden es einen fehler gibt?
    -- var = e1 op e2
    Assign var (EBinOp op e1 e2) ->
      let e1' = evalExpr e1 ctxt -- evalExpr nötig, damit Variablen zum Zeitpunkt, an dem das Assignment auftritt, ihren neu zugewiesenen Wert erhalten
          e2' = evalExpr e2 ctxt -- und dies im Kontext auch so übernommen wird, sodass man den zugewiesenen Wert direkt aus dem Kontext lesen kann
       in ( Assign var (EBinOp op e1' e2') -- wird sowohl im Statement als auch im Context übernommen
          , updateOneAV var (ATerm (EBinOp op e1' e2')) ctxt)
    Assign var expr -> (Assign var expr, updateOneAV var (ATerm expr) ctxt) -- EBool, EInt, EFloat werden einfach so übernommen
    Send v -> (setInContext "send" ctxt v, ctxt)
    Receive v -> (setInContext "recv" ctxt v, ctxt)
    End v -> (setInContext "end" ctxt v, ctxt)
    Go s1 s2 ->
      let (s1', ctxt1) = stmtToST ctxt s1
          (s2', ctxt2) = stmtToST ctxt1 s2
       in (Go s1' s2', ctxt2)
    For hdr s ->
      let (s', ctxt') = stmtToST ctxt s
       in (For hdr s', ctxt')
    _ -> (st, ctxt)


-- evaluates conditions and considers the context when there is a send/receive

stmtToST' :: Context -> Statement -> Either String (Statement, Context)
stmtToST' ctxt st =
  case st of
    New c s ->
      case stmtToST' ctxt s of
        Right (s', ctxt') -> Right (New c s', ctxt')
        Left err          -> Left err
    Sequence s1 s2 ->
      case stmtToST' ctxt s1 of
        Right (s1', ctxt1) ->
          case stmtToST' ctxt1 s2 of
            Right (s2', ctxt2) -> Right (Sequence s1' s2', ctxt2)
            Left err           -> Left err
        Left err -> Left err
    If e s1 s2 ->
      case stmtToST' ctxt s1 of
        Right (s1', ctxt1) ->
          case stmtToST' ctxt s2 of
            Right (s2', ctxt2) ->
              let e' = evalExpr e ctxt -- auch hier evalExpr, damit Variablen aus der Bedingung ihren aktuellen Wert aus dem AV im Context bekommen
                  ctxtMerged = mergeIfContexts e' ctxt1 ctxt2
               in case checkTypeOfExpression TBool e' ctxt of
                    True -> Right (If e' s1' s2', ctxtMerged)
                    False ->
                      Left
                        $ "Type error: condition expression not of type Bool in If statement."
            Left err -> Left err
        Left err -> Left err
    -- var = x
    Assign var (EVar x) ->
      case lookupAV x ctxt of
        ATerm e ->
          case typeCheck (updateOneAV var (ATerm e) ctxt) of
            Right () -> Right (Assign var e, (updateOneAV var (ATerm e) ctxt)) -- var bekommt den Term der x definiert
            Left err -> Left err
        AIf e av1 av2 ->
          case typeCheck (updateOneAV var (AIf e av1 av2) ctxt) of
            Right () ->
              Right
                (Assign var (EVar x), (updateOneAV var (AIf e av1 av2) ctxt)) -- var bekommt die AIf condition von x
            Left err -> Left err
        _ ->
          case typeCheck (updateOneAV var (ATerm (EVar x)) ctxt) of
            Right _ ->
              Right
                (Assign var (EVar x), (updateOneAV var (ATerm (EVar x)) ctxt))
    Assign var (EBinOp op e1 e2) ->
      case typeCheck (updateOneAV var (ATerm (EBinOp op e1 e2)) ctxt) of
        Right () ->
          let e1' = evalExpr e1 ctxt -- evalExpr nötig, damit Variablen zum Zeitpunkt, an dem das Assignment auftritt, ihren neu zugewiesenen Wert erhalten
              e2' = evalExpr e2 ctxt -- und dies im Kontext auch so übernommen wird
           in Right
                ( Assign var (EBinOp op e1' e2')
                , (updateOneAV var (ATerm (EBinOp op e1' e2')) ctxt)) -- wird sowohl im Statement als auch im Context übernommen
        Left err -> Left err
    Assign var expr ->
      case typeCheck (updateOneAV var (ATerm expr) ctxt) of
        Right () -> Right (Assign var expr, (updateOneAV var (ATerm expr) ctxt)) -- EBool, EInt, EFloat werden einfach so übernommen
        Left err -> Left err
    Send v -> Right (setInContext "send" ctxt v, ctxt)
    Receive v -> Right (setInContext "recv" ctxt v, ctxt)
    End v -> Right (setInContext "end" ctxt v, ctxt)
    Go s1 s2 ->
      case stmtToST' ctxt s1 of
        Left err -> Left err
        Right (s1', ctxt1) ->
          case stmtToST' ctxt1 s2 of
            Left err           -> Left err
            Right (s2', ctxt2) -> Right (Go s1' s2', ctxt2)
    For hdr s ->
      case stmtToST' ctxt s of
        Right (s', ctxt') -> Right (For hdr s', ctxt')
        Left err          -> Left err
    _ -> Right (st, ctxt)

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
                    _ -> Left "no int detected in comparison"
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
evalExpr e _ = e


-- used for "Send v"/"Receive v/End v" to trace
setInContext :: String -> Context -> VarName -> Statement
setInContext mode ctxt v =
  case lookupAV v ctxt of
    AIf cond (ATerm (EVar v1)) (ATerm (EVar v2)) ->
      let act m var =
            if m == "send"
              then Send var
              else if m == "recv"
                     then Receive var
                     else End var
       in if v1 == v2
            then act mode v1
            else If cond (act mode v1) (act mode v2)
    ATerm (EVar x) -- Es kann sehr wohl ein Channel im ATerm enthalten sein, wenn unser AV die Form ATerm (Evar x) hat und einem Channel zugewiesen ist
     ->
      let act m var =
            if m == "send"
              then Send var
              else if m == "recv"
                     then Receive var
                     else End var
       in act mode x
    -- Fallback
    _ ->
      if mode == "send"
        then Send v
        else if mode == "recv"
               then Receive v
               else End v


-- representing a parsed Statement as a Session Type, optionally used after stmtToST!
prettyPrintST :: Statement -> String
prettyPrintST x =
  case x of
    New (VarName c) s -> "new " ++ c ++ "." ++ prettyPrintST s
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
    If e s1 s2 -> block s1 ++ " if " ++ show e ++ " else " ++ block s2
    Go s1 s2 ->
      "go" ++ "{" ++ prettyPrintST s1 ++ "}" ++ "{" ++ prettyPrintST s2 ++ "}"
    Assign _ _ -> ""
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


-- in the beginning all the Abstract Values are unknown
initialContext :: VarDecs -> Context
initialContext decs = freshInitialContext (Map.fromList [((x), (y, AUnknown)) | (x, y) <- decs])

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
            abstractEq AUnknown AUnknown     = True
            abstractEq _ _                   = False


-- flips the directions of all communications
dual :: Statement -> Statement
dual (Send var)       = Receive var
dual (Receive var)    = Send var
dual (New var s)      = New var (dual s)
dual (Sequence s1 s2) = Sequence (dual s1) (dual s2)
dual (If e s1 s2)     = If e (dual s1) (dual s2)
dual (For head s)     = For head (dual s)
dual (Go s1 s2)       = Go (dual s1) (dual s2)
-- Assign, Skip End bleiben unverändert
dual x                = x


-- go, for, new, some special if cases -> skip
strip :: Statement -> Statement
strip (Go s1 s2) = Skip
strip (For head s1) = Skip
strip (New var s) = strip s
strip (Assign var e) = Skip
strip (Sequence s1 s2) = Sequence (strip s1) (strip s2)
strip (If e s1 s2) =
  case (strip s1, strip s2) of
    (Skip, Skip) -> Skip -- if ... then assign... else assign...
    (s1', s2')   -> If e s1' s2'
strip x = x

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
