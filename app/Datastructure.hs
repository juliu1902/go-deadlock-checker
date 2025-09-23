module Datastructure where  
import qualified Data.Map as Map (Map, fromList, lookup, insert, lookupMin, deleteMin, empty, map, delete)
import Control.Monad.State
import Control.Monad(replicateM)
data Statement = New VarName Statement | Skip | Send VarName | Receive VarName | End VarName | Sequence Statement Statement | If Expr Statement Statement | For ForHeader Statement | Assign VarName Expr | Go Statement Statement deriving (Show)
-- new types for variable assignments and the list of variable declarations
newtype VarName = VarName String deriving (Eq, Ord)
newtype ChannelID = ChannelID String deriving (Show, Eq) -- type or newtype tbd
type FreshM = State Int -- State Int Monad for unique channelID naming
data ForHeader = ForHeaderRunning VarName Int Expr IncDec | ForHeaderRange VarName VarName deriving (Show) 
data IncDec = Inc | Dec

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
-- -- finds AV of a variable
lookupAV :: VarName -> Context -> AbstractVal
lookupAV x ctxt = maybe AUnknown snd (Map.lookup x ctxt)

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
    New _ _   -> ctxt
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

-- representing the parsed Statement as a Session Type
stmtToST :: Statement -> String
stmtToST x = case x of
    New (VarName c) s         -> "new " ++ c ++ "." ++ stmtToST s
    Skip                        -> "skip"
    Send (VarName ch)         -> ch ++ "!"
    Receive (VarName ch)      -> ch ++ "?"
    End (VarName ch)          -> ch ++ "#"
    Sequence s1 s2 ->
      let a = stmtToST s1
          b = stmtToST s2
      -- assignments werden nicht mit ; getrennt sondern ignoriert
      in case (null a, null b) of
        (True,  True)  -> ""
        (True,  False) -> b
        (False, True)  -> a
        (False, False) -> a ++ ";" ++ b
    If _ (Assign _ _) (Assign _ _) -> "" -- ifs mit nur assigns werden ignoriert
    If e s1 s2     -> block s1 ++ " if " ++ show e ++ " else " ++ block s2
    Go s1 s2       -> "go" ++ "{" ++ stmtToST s1 ++ "}" ++ "{" ++ stmtToST s2 ++ "}"
    Assign _ _     -> ""
    For (ForHeaderRunning var start e incdec) s -> "for " ++ "(" ++ show var ++ "=" ++ show start ++ ";" ++ show e ++ ";" ++ show var ++ show incdec ++ ") " ++ block s
    For (ForHeaderRange var chan) s -> "for " ++ "(" ++ show var ++ " := range " ++ show chan ++ " " ++ show Skip
    where 
        block :: Statement -> String
        block st@(Sequence _ _) = "{" ++ stmtToST st ++ "}"
        block st = stmtToST st

-- flips the directions of all communications
dual :: Statement -> Statement
dual (Send var) = Receive var
dual (Receive var) = Send var 
dual (New var s) = New var (dual s)
dual (Sequence s1 s2) = Sequence (dual s1) (dual s2)
dual (If e s1 s2) = If e (dual s1) (dual s2)
dual (For head s) = For head (dual s)
dual (Go s1 s2) = Go (dual s1) (dual s2)
dual x = x
-- dual (Assign var e) = Assign var e
-- dual (Skip) = Skip
-- dual (End var) = End var