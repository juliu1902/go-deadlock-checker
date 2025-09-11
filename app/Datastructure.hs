{-# LANGUAGE GADTs #-}
module Datastructure where

data Statement = New ChannelID Statement | Skip | Send ChannelID | Receive ChannelID | End ChannelID | Sequence Statement Statement | If Expr Statement Statement | For Expr Statement | Assign VarName AbstractVal | Go Statement Statement deriving (Show)

-- new types for variable assignments and the list of variable declarations
newtype VarName = VarName String
newtype ChannelID = ChannelID String deriving (Show) -- type or newtype tbd
data ChanType = CInt | CBool deriving (Show)
data VarType = TInt | TBool | TChan ChanType deriving (Show)
type VarDec = (String, VarType)
type VarDecs = [VarDec]

data AbstractVal = Achan ChannelID                        -- Kanalname
                | Aif Expr       AbstractVal AbstractVal  -- eine Auswahl zwischen verschiedenen Abstract Values
                | Aterm Expr                              -- Ein Ausdruck, der definitiv keinen Kanal enthält
                | Aunknown                                -- noch unbekannt
                deriving (Show)                         

data Expr = EVar VarName 
  | EBool Bool             
  | EInt Integer          
  | EFloat Double           
  | EBinOp BinOp Expr Expr

data BinOp = Add | Sub | Mul | Div | Mod | Gt | Lt | Ge | Le | Eq | Neq | And | Or

data Program = Program VarDecs Statement

-- type Context = [(VarName, (VarType, AbstractVal))]
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
    Gt -> ">"
    Lt -> "<"
    Ge -> ">="
    Le -> "<="
    Eq -> "=="
    Neq -> "!="
    And -> "&&"
    Or -> "||"

-- representing the parsed Statement as a Session Type
stmtToST :: Statement -> String
stmtToST x = case x of
    New (ChannelID c) s        -> "new " ++ c ++ "." ++ stmtToST s
    Skip           -> "skip"
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
    If e s1 s2     -> block s1 ++ " if " ++ show e ++ " else " ++ block s2
    For e s        -> "for " ++ show e ++ " " ++ block s
    Go s1 s2       -> "go" ++ "{" ++ stmtToST s1 ++ "}" ++ "{" ++ stmtToST s2 ++ "}"
    Assign _ _     -> ""
    where 
        block :: Statement -> String
        block st@(Sequence _ _) = "{" ++ stmtToST st ++ "}"
        block st = stmtToST st
