{-# LANGUAGE GADTs #-}
module Datastructure where

data Statement = New ChannelID Statement | Skip | Send ChannelID | Receive ChannelID | End ChannelID | Sequence Statement Statement | If Expr Statement Statement | For Expr Statement | Assign VarName AbstractVal | Go Statement Statement deriving (Show)
type Expr = String

-- new types for variable assignments and the list of variable declarations
newtype VarName = VarName String deriving (Show)
type ChannelID = String -- type or newtype tbd

data VarType = TInt | TBool | TChan deriving (Show)
type VarDec = (String, VarType) 

data AbstractVal = Achan ChannelID                        -- Kanalname
                | Aif Expression AbstractVal AbstractVal  -- eine Auswahl zwischen verschiedenen Abstract Values
                | Aterm Expression                        -- Ein Ausdruck, der definitiv keinen Kanal enthält
                | Aunknown                                -- noch unbekannt
                deriving (Show)                         

data Expression = EVar VarName 
  | EBool Bool             
  | EInt Integer          
  | EFloat Double           
  | EBinOp BinOp Expression Expression
  deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod | Gt | Lt | Ge | Le | Eq | Neq | And deriving (Show)

type Context = [(VarName, (VarType, AbstractVal))]
-- TChan kommt nur im kontext als tupel mit Achan oder Aif vor. TBool und TInt nur bei Aterm, sonst ist das Assignment ungültig?
-- not in use yet

-- representing the parsed Statement as a Session Type
stmtToST :: Statement -> String
stmtToST x = case x of
    New c s        -> "new " ++ c ++ "." ++ stmtToST s
    Skip           -> "skip"
    Send ch         -> ch ++ "!"
    Receive ch      -> ch ++ "?"
    End ch          -> ch ++ ".end"
    Sequence s1 s2 ->  stmtToST s1 ++ ";" ++ stmtToST s2
    If e s1 s2     -> block s1 ++ " if " ++ e ++ " else " ++ block s2
    For e s        -> "for " ++ e ++ " " ++ block s
    Go s1 s2       -> "go" ++ "{" ++ stmtToST s1 ++ "}" ++ "{" ++ stmtToST s2 ++ "}"
    Assign _ _     -> ""
    where 
        block :: Statement -> String
        block st@(Sequence _ _) = "{" ++ stmtToST st ++ "}"
        block st = stmtToST st
