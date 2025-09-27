module Datastructure where  
import qualified Data.List as List
import qualified Data.Map as Map (Map, fromList, lookup, insert, lookupMin, deleteMin, empty, map, delete, toList)
import Control.Monad.State
import Control.Monad(replicateM)
data Statement = New VarName Statement | Skip | Send VarName | Receive VarName | End VarName | Sequence Statement Statement | If Expr Statement Statement | For ForHeader Statement | Assign VarName Expr | Go Statement Statement deriving (Show, Eq)
-- new types for variable assignments and the list of variable declarations
newtype VarName = VarName String deriving (Eq, Ord)
newtype ChannelID = ChannelID String deriving (Show, Eq) -- type or newtype tbd
type FreshM = State Int -- State Int Monad for unique channelID naming
data ForHeader = ForHeaderRunning VarName Int Expr IncDec | ForHeaderRange VarName VarName deriving (Show, Eq) 
data IncDec = Inc | Dec deriving (Eq)

instance Show (IncDec) where
  show Inc = "++"
  show Dec = "--"

data ChanType = CInt | CBool deriving (Show)
data VarType = TInt | TBool | TChan ChanType deriving (Show)
type VarDec = (VarName, VarType)
type VarDecs = [VarDec]

data AbstractVal = AChan ChannelID                        -- Kanalname
                | AIf Expr AbstractVal AbstractVal        -- eine Auswahl zwischen verschiedenen Abstract Values
                | ATerm Expr                              -- Ein Ausdruck, der definitiv keinen Kanal enthält
                | AUnknown                                -- noch unbekannt
                deriving (Show)                         

data Expr = EVar VarName 
  | EBool Bool             
  | EInt Integer          
  | EFloat Double           
  | EBinOp BinOp Expr Expr deriving (Eq)

data BinOp = Add | Sub | Mul | Div | Mod | Gt | Lt | Ge | Le | Eq | Neq | And | Or deriving (Eq)

type Context = Map.Map VarName (VarType, AbstractVal) -- datatype for the inferContext function which evaluates all abstract values
data Program = Program VarDecs Statement -- Parser output datatype of parseProgram


instance Show VarName where
  show (VarName x) = x

instance Show Expr where
  show e = case e of
    EVar x -> show x
    EBool x -> show x
    EInt x -> show x
    EFloat x -> show x
    EBinOp op e1 e2 -> show e1 ++ show op ++ show e2

instance Show BinOp where
  show op = case op of
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
stmtToST :: Context -> Statement -> Statement
stmtToST ctxt st = case st of
  New c s -> New c (stmtToST ctxt s)
  Sequence s1 s2 -> Sequence (stmtToST ctxt s1) (stmtToST ctxt s2)
  If e s1 s2 -> If (evaluateExpr ctxt e) (stmtToST ctxt s1) (stmtToST ctxt s2)
  Send var -> setInContext "send" var
  Receive var -> setInContext "recv" var
  x -> x
  where
    setInContext :: String -> VarName -> Statement
    setInContext mode v =
      case lookupAV v ctxt of
        AIf cond (AChan c1) (AChan c2) -> if c1 == c2 then setInContext mode (head (lookupVarNamesForChannel c1 ctxt)) else If (evaluateExpr ctxt cond) (if mode == "send" then Send (head (lookupVarNamesForChannel c1 ctxt)) else Receive (head (lookupVarNamesForChannel c1 ctxt))) (if mode == "send" then Send (head (lookupVarNamesForChannel c2 ctxt)) else Receive (head(lookupVarNamesForChannel c2 ctxt)))
        AIf cond av1 av2 -> if mode == "send" then Send v else Receive v
        _ -> if mode == "send" then Send v else Receive v

-- representing a parsed Statement as a Session Type, optionally used after stmtToST!
prettyPrintST :: Statement -> String
prettyPrintST x = case x of
    New (VarName c) s         -> "new " ++ c ++ "." ++ prettyPrintST s
    Skip                        -> "skip"
    Send (VarName ch)         -> ch ++ "!"
    Receive (VarName ch)      -> ch ++ "?"
    End (VarName ch)          -> ch ++ "#"
    Sequence s1 s2 ->
      let a = prettyPrintST s1
          b = prettyPrintST s2
      -- assignments werden nicht mit ; getrennt sondern ignoriert
      in case (null a, null b) of
        (True,  True)  -> ""
        (True,  False) -> b
        (False, True)  -> a
        (False, False) -> a ++ ";" ++ b
    If _ (Assign _ _) (Assign _ _) -> "" -- ifs mit nur assigns werden ignoriert
    If e s1 s2     -> block s1 ++ " if " ++ show e ++ " else " ++ block s2
    Go s1 s2       -> "go" ++ "{" ++ prettyPrintST s1 ++ "}" ++ "{" ++ prettyPrintST s2 ++ "}"
    Assign _ _     -> ""
    For (ForHeaderRunning var start e incdec) s -> "for " ++ "(" ++ show var ++ "=" ++ show start ++ ";" ++ show e ++ ";" ++ show var ++ show incdec ++ ") " ++ block s
    For (ForHeaderRange var chan) s -> "for " ++ "(" ++ show var ++ " := range " ++ show chan ++ " " ++ show Skip
    where 
        block :: Statement -> String
        block st@(Sequence _ _) = "{" ++ prettyPrintST st ++ "}"
        block st = prettyPrintST st

-- generates the unique channelID names 
freshChannel :: FreshM ChannelID
freshChannel = do
  n <- get
  put (n+1)
  return $ ChannelID ("id" ++ show n)

-- in the beginning all the Abstract Values are unknown
initialContext :: VarDecs -> Context
initialContext decs = Map.fromList [ ((x), (y, AUnknown)) | (x, y) <- decs]
-- except for the channel type annotations in the beginning
-- var chan int/bool should automatically create fresh channels with unique channelID
freshInitialContext :: Context -> Context
freshInitialContext initialc = evalState (traverse freshOne initialc) 0 where -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
  freshOne :: (VarType, AbstractVal) -> FreshM (VarType, AbstractVal)                      --                                               freshOne     ctxt    FreshM (VarType, AbstractVal)
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

-- returns variable that points to a specific channelID
lookupVarNamesForChannel :: ChannelID -> Context -> [VarName]
lookupVarNamesForChannel ch ctxt = [ var | (var, (_, AChan ch')) <- Map.toList ctxt, ch == ch']
-- writes one AbstractVal-update in current ctxt
updateOneAV :: VarName -> AbstractVal -> Context -> Context
updateOneAV x av ctxt = case Map.lookup x ctxt of
  Nothing -> ctxt
  Just (t, _) -> Map.insert x (t, av) ctxt

-- the analysed "if-then-else branch" brought two contexts
-- mergeIfContexts merges them into one
mergeIfContexts :: Expr -> Context -> Context -> Context
mergeIfContexts cond c1 c2 = case Map.lookupMin c1 of
  Nothing -> Map.map (\(t, av2) -> (t, AIf cond AUnknown av2)) c2 -- values in c2 that have not been analyzed yet are considered!
  Just (k, (t, av)) -> case Map.lookup k c2 of
    Nothing -> Map.insert k (t, AIf cond av AUnknown) (mergeIfContexts cond (Map.deleteMin c1) c2)
    Just (t2, av2) -> if abstractEq av av2 
      then Map.insert k (t, av) (mergeIfContexts cond (Map.deleteMin c1) (Map.delete k c2))
      else Map.insert k (t, AIf cond av av2) (mergeIfContexts cond (Map.deleteMin c1) (Map.delete k c2)) -- it's required for abs and abs2 to have the same VarType!
    where
      abstractEq :: AbstractVal -> AbstractVal -> Bool
      abstractEq (AChan c1) (AChan c2) = c1 == c2
      abstractEq (ATerm e1) (ATerm e2) = e1 == e2
      abstractEq AUnknown AUnknown = True
      abstractEq _ _ = False


-- takes the variable declarations and the parsed statement and 
-- extracts all abstract values out of the program
inferContext :: VarDecs -> Statement -> Context
inferContext decs stmt = inferStmt (freshInitialContext (initialContext decs)) stmt  where -- evalState :: State s a -> s -> a, 0 is our starting state s, a Context will be returned (-> a)
  inferStmt :: Context -> Statement -> Context
  inferStmt ctxt st = case st of
    New _ s   -> inferStmt ctxt s
    Send _    -> ctxt
    Go  _ _   -> ctxt
    Skip      -> ctxt
    End _     -> ctxt
    Receive _ -> ctxt
    For _ _   -> ctxt

    Sequence s1 s2 -> inferStmt (inferStmt ctxt s1) s2
    Assign var (EVar x) -> case lookupType x ctxt of
      Just (TChan y) -> (updateOneAV (var) (lookupAV x ctxt) ctxt) -- var = x -> var needs to have the AV of x if x exists in ctxt, else AUnknown
      _ -> (updateOneAV var (ATerm (EVar x)) ctxt) -- var = x und x ist int oder bool, definitiv kein Kanal
    Assign var x -> updateOneAV var (ATerm x) ctxt
    If cond s1 s2 -> mergeIfContexts cond (inferStmt ctxt s1) (inferStmt ctxt s2)

-- flips the directions of all communications
dual :: Statement -> Statement
dual (Send var) = Receive var
dual (Receive var) = Send var 
dual (New var s) = New var (dual s) 
dual (Sequence s1 s2) = Sequence (dual s1) (dual s2)
dual (If e s1 s2) = If e (dual s1) (dual s2)
dual (For head s) = For head (dual s)
dual (Go s1 s2) = Go (dual s1) (dual s2)
-- Assign, Skip End bleiben unverändert
dual x = x

-- substitutes all variables in an expression with a context
evaluateExpr :: Context -> Expr -> Expr 
evaluateExpr ctxt (EVar x) = case (Map.lookup x ctxt) of
  Just (t, ATerm x) -> evaluateExpr ctxt x
  _ -> (EVar x)
evaluateExpr ctxt (EBinOp op e1 e2) = EBinOp op (evaluateExpr ctxt e1) (evaluateExpr ctxt e2)
-- float int bool unverändert
evaluateExpr ctxt x = x

-- go, for, new -> skip
strip :: Statement -> Statement
strip (Go s1 s2) = Skip 
strip (For head s1) = Skip
strip (New var s) = strip s
strip (Assign var e) = Skip
strip (Sequence s1 s2) = Sequence (strip s1) (strip s2)
-- Rest bleibt so wie es ist
strip x = x

assocIdRules :: Statement -> Statement
assocIdRules (Sequence (Skip) s) = s
assocIdRules (Sequence s (Skip)) = s
assocIdRules (Sequence (Sequence s1 s2) s3) = Sequence s1 (Sequence s2 s3)
assocIdRules x = x
-- idFor nicht nötig, for wird durch strip zu skip

condEta :: Statement -> Statement
condEta (If e s1 s2) = if s1 == s2 then s1 else (If e s1 s2)
condEta x = x

-- assoc, id, condeta is applied once on all "nodes"
applyFirstLevel :: Statement -> Statement 
applyFirstLevel stmt = case stmt of
  Sequence s1 s2 -> assocIdRules (Sequence (applyFirstLevel s1) (applyFirstLevel s2))
  If e s1 s2 -> condEta (If e (applyFirstLevel s1) (applyFirstLevel s2))
  _ -> stmt

-- applies a rule until it doesn't change the input anymore
repeatApply :: (Statement -> Statement) -> Statement -> Statement
repeatApply f stmt = if (f stmt) == stmt then stmt else repeatApply f stmt

phaseA :: Statement -> Statement
phaseA stmt = repeatApply applyFirstLevel stmt

condDist :: Statement -> Statement 
condDist (Sequence (If e s1 s2) s) = If e (Sequence s1 s) (Sequence s2 s)
condDist x = x

-- apply conddist once on all "nodes"
applySecondLevel :: Statement -> Statement
applySecondLevel stmt = case stmt of
  Sequence s1 s2 -> condDist (Sequence (applySecondLevel s1) (applySecondLevel s2))
  If e s1 s2 -> If e (applySecondLevel s1) (applySecondLevel s2)
  _ -> stmt 

normalizeST :: Statement -> Statement
normalizeST stmt = normalize (strip stmt) where 
  normalize stmt =
    let stmtA = phaseA stmt -- Sie wenden die assoc, die id-Regeln und cond-eta so lange an, bis nichts mehr geht.     
        stmtB = applySecondLevel stmtA  
    in if (stmtA == stmtB)  -- ändert condDist unseren ST?
      then stmtA               -- Nein, dann keine Regel mehr anwendbar
      else normalize stmtB  -- Ja, dann einmal cond-dist und wieder assoc, id, cond-eta usw bis keine dieser Regeln mehr anwendbar ist

testEquivalence :: Statement -> Statement -> Bool
testEquivalence s t = case (s, t) of
  (Send x,    Send y)      -> x == y -- atom-send
  (Receive x, Receive y)   -> x == y -- atom-recv
  (End x,     End y)       -> x == y -- atom-end
  -- comp-rule s1 ~ s2 /\ s3 ~ s4 => s1; s3 ~ s2; s4
  (Sequence s1 s3, Sequence s2 s4) -> testEquivalence s1 s2 && testEquivalence s3 s4
  -- cond-rule s1 ~ s2 /\ s3 ~ s4 => if (e) {s1}{ s3} ~ if (e) {s2}{ s4}
  (If e s1 s3, If e' s2 s4) -> e == e' && testEquivalence s1 s2 && testEquivalence s3 s4
  -- else false
  _ -> False