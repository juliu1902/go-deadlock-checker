{-# LANGUAGE GADTs #-}
module Datastructure where  
import qualified Data.Map as Map (Map, fromList, lookup, insert, lookupMin, deleteMin, empty, map, delete)

data Statement = New ChannelID Statement | Skip | Send ChannelID | Receive ChannelID | End ChannelID | Sequence Statement Statement | If Expr Statement Statement | For Expr Statement | Assign VarName Expr | Go Statement Statement deriving (Show)

-- new types for variable assignments and the list of variable declarations
newtype VarName = VarName String deriving (Eq, Ord)
newtype ChannelID = ChannelID String deriving (Show, Eq) -- type or newtype tbd
data ChanType = CInt | CBool deriving (Show)
data VarType = TInt | TBool | TChan ChanType deriving (Show)
type VarDec = (String, VarType)
type VarDecs = [VarDec]

data AbstractVal = Achan ChannelID                        -- Kanalname
                | Aif Expr AbstractVal AbstractVal  -- eine Auswahl zwischen verschiedenen Abstract Values
                | Aterm Expr                              -- Ein Ausdruck, der definitiv keinen Kanal enthält
                | Aunknown                                -- noch unbekannt
                deriving (Show)                         

data Expr = EVar VarName 
  | EBool Bool             
  | EInt Integer          
  | EFloat Double           
  | EBinOp BinOp Expr Expr deriving (Eq)

data BinOp = Add | Sub | Mul | Div | Mod | Gt | Lt | Ge | Le | Eq | Neq | And | Or deriving (Eq)

data Program = Program VarDecs Statement
-- TChan kommt nur im kontext als tupel mit Achan oder Aif vor. TBool und TInt nur bei Aterm, sonst ist das Assignment ungültig?
-- not in use yet

instance Show VarName where
  show (VarName x) = x

instance Show Expr where
  show e = case e of
    EVar x -> show x
    EBool x -> show x
    EInt x -> show x
    EFloat x -> show x
    EBinOp op e1 e2 -> "(" ++ show e1 ++ show op ++ show e2 ++ ")"

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


type Context = Map.Map VarName (VarType, AbstractVal)

-- in the beginning all the Abstract Values are unknown!
initialContext :: VarDecs -> Context
initialContext decs = Map.fromList [ ((VarName x), (y, Aunknown)) | (x, y) <- decs] 

-- finds vartype of a variable
lookupType :: VarName -> Context -> Maybe VarType
lookupType x ctxt = fmap fst (Map.lookup x ctxt)

-- writes one AbstractVal-update in current ctxt
updateOneAV :: VarName -> AbstractVal -> Context -> Context
updateOneAV x av ctxt = case Map.lookup x ctxt of
  Nothing -> ctxt
  Just (t, _) -> Map.insert x (t, av) ctxt

-- the analysed "if-then-else branch" brought two contexts
-- mergeIfContexts merges them into one
mergeIfContexts :: Expr -> Context -> Context -> Context
mergeIfContexts cond c1 c2 = case Map.lookupMin c1 of
  Nothing -> Map.map (\(t, av2) -> (t, Aif cond Aunknown av2)) c2 -- values in c2 that have not been analyzed yet are considered!
  Just (k, (t, av)) -> case Map.lookup k c2 of
    Nothing -> Map.insert k (t, Aif cond av Aunknown) (mergeIfContexts cond (Map.deleteMin c1) c2)
    Just (t2, av2) -> if abstractEq av av2 
      then Map.insert k (t, av) (mergeIfContexts cond (Map.deleteMin c1) (Map.delete k c2))
      else Map.insert k (t, Aif cond av av2) (mergeIfContexts cond (Map.deleteMin c1) (Map.delete k c2)) -- it's required for abs and abs2 to have the same VarType!
    where
      abstractEq :: AbstractVal -> AbstractVal -> Bool
      abstractEq (Achan c1) (Achan c2) = c1 == c2
      abstractEq (Aterm e1) (Aterm e2) = e1 == e2
      abstractEq Aunknown Aunknown = True
      abstractEq _ _ = False


-- takes the variable declarations and the parsed statement and 
-- extracts all abstract values out of the program
inferContext :: VarDecs -> Statement -> Context
inferContext decs stmt = inferStmt (initialContext decs) stmt where
  inferStmt :: Context -> Statement -> Context
  inferStmt ctxt st = case st of
    -- the "easy" cases
    Send _   -> ctxt
    New chan@(ChannelID c) s1  -> inferStmt (updateOneAV (VarName c) (Achan chan) ctxt) s1
    Go  _ _   -> ctxt  -- ?
    Skip      -> ctxt
    End _     -> ctxt
    Receive _ -> ctxt
    For _ _   -> ctxt -- TODO - sollte so nicht behandelt werden, weiß noch nicht mit for umzugehen

    Sequence s1 s2 -> inferStmt (inferStmt ctxt s1) s2
    Assign id val -> case val of -- c = c1 -> name = c, c1 = EVar c1
      EVar name@(VarName x) -> case lookupType name ctxt of -- lookup the VarType of c1
        Just (TChan y) -> updateOneAV (id) (Achan (ChannelID x)) ctxt
        _              -> updateOneAV (id) (Aterm val) ctxt
    
    If cond s1 s2 -> mergeIfContexts cond (inferStmt ctxt s1) (inferStmt ctxt s2)
-- If Expr Statement Statement | For Expr Statement |deriving (Show)




-- representing the parsed Statement as a Session Type
stmtToST :: Statement -> String
stmtToST x = case x of
    New (ChannelID c) s         -> "new " ++ c ++ "." ++ stmtToST s
    Skip                        -> "skip"
    Send (ChannelID ch)         -> ch ++ "!"
    Receive (ChannelID ch)      -> ch ++ "?"
    End (ChannelID ch)          -> ch ++ ".end"
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
    For e s        -> "for " ++ show e ++ " " ++ block s
    Go s1 s2       -> "go" ++ "{" ++ stmtToST s1 ++ "}" ++ "{" ++ stmtToST s2 ++ "}"
    Assign _ _     -> ""
    where 
        block :: Statement -> String
        block st@(Sequence _ _) = "{" ++ stmtToST st ++ "}"
        block st = stmtToST st
