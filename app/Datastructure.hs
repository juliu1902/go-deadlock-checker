{-# LANGUAGE GADTs #-}
module Datastructure where
import Data.List

--  s ::= skip | ! | ? | end | s;s | s if e else s | for e s
--  s ::= new c. s | skip | c! | c? | c.end | s;s | s if e else s | for e s | go s s
data Statement = New Channel Statement | Skip | Send Channel | Receive Channel | End Channel | Sequence Statement Statement | If Expr Statement Statement | For Expr Statement | Assign VarName AbstractVal | Go Statement Statement
type Expr = String

-- new types for variable assignments and the list of variable declarations
newtype VarName = VarName String
newtype ChannelID = ChannelID String

data VarType = TInt | TBool | TChan
type VarDec = (String, VarType)

data AbstractVal = Achan ChannelID                        -- Kanalname
                | Aif Expression AbstractVal AbstractVal  -- eine Auswahl zwischen verschiedenen Abstract Values
                | Aterm Expression                        -- Ein Ausdruck, der definitiv keinen Kanal enthält
                | Aunknown                                -- noch unbekannt

data Expression = EVar VarName 
  | EBool Bool             
  | EInt Integer          
  | EFloat Double           
  | EBinOp BinOp Expression Expression

data BinOp = Add | Sub | Mul | Div | Mod | Gt | Lt | Ge | Le | Eq | Neq | And

type Context = [(VarName, (VarType, AbstractVal))]
-- TChan kommt nur im kontext als tupel mit Achan oder Aif vor. TBool und TInt nur bei Aterm, sonst ist das Assignment ungültig?
-- not in use yet


type Channel = String
newtype ChannelBehavior = ChannelBehavior [(Channel, Statement)]

instance Show Statement where
  show x = case x of
    New c s -> "new " ++ c ++ show s
    Skip           -> "skip"
    Send _         -> "!"
    Receive _      -> "?"
    End _          -> "end"
    Sequence s1 s2 ->  show s1 ++ ";" ++ show s2
    If e s1 s2     -> show s1 ++ " if " ++ e ++ " else " ++ show s2
    For e s        -> "for " ++ e ++ " " ++ show s
    Go s1 s2       -> "go" ++ "{" ++ show s1 ++ "}" ++ "{" ++ show s2 ++ "}"
    Assign _ _     -> ""

instance Show ChannelBehavior where
    show (ChannelBehavior []) = ""
    show (ChannelBehavior(x:xs)) = fst x ++ " |-> " ++ show (snd x) ++ "\n" ++ show (ChannelBehavior xs)

-- inserts new Channel Statement pair in given ChannelBehavior or adds it with Sequence to already existing Channel
insertStmt :: ChannelBehavior -> (Channel, Statement) -> ChannelBehavior
insertStmt  (ChannelBehavior [])(ch, st) = ChannelBehavior [(ch, st)]
insertStmt  (ChannelBehavior (x:xs))(ch, st)
    | ch == fst x = ChannelBehavior ((ch, Sequence (snd x) st):xs)
    | otherwise =
        let ChannelBehavior rest = insertStmt (ChannelBehavior xs) (ch, st)
        in ChannelBehavior (x : rest)

assignChannelsToStmts :: Statement -> ChannelBehavior
assignChannelsToStmts stmt =
    let tuples = helper stmt
    in foldl insertStmt (ChannelBehavior []) tuples -- merging multiple (Channel, Statement)-pairs with the same channelname to one single (Channel, Statement)
    where
        -- helper function takes a Statement and the channel of the last detected action (mainly necessary for skip)
        -- and creates its many (Channel,Statement) pairs (!! without grouping the same channels into one single (Channel, Statement) pair at first!!)
        helper :: Statement -> [(Channel, Statement)]
        helper s = case s of
            Send ch      -> [(ch, s)]
            Receive ch   -> [(ch, s)]
            End ch       -> [(ch, s)]
            Skip         -> [] -- sollte nicht auftreten
            Sequence s1 s2 ->
                let res1 = helper s1
                    res2 = helper s2
                in res1 ++ res2
            If cond s1 s2 ->
                let grouped1 = foldl insertStmt (ChannelBehavior []) (helper s1)
                    grouped2 = foldl insertStmt (ChannelBehavior []) (helper s2)
                in wrapIf (pairIf grouped1 grouped2) cond where
                    pairIf :: ChannelBehavior -> ChannelBehavior -> [(Channel, (Maybe Statement, Maybe Statement))]
                    pairIf (ChannelBehavior xs) (ChannelBehavior ys) =
                      [ (ch, (lookup ch xs, lookup ch ys)) | ch <- nub (map fst xs ++ map fst ys) ]
                    wrapIf :: [(Channel, (Maybe Statement, Maybe Statement))] -> Expr -> [(Channel, Statement)]
                    wrapIf [] _ = []
                    wrapIf ((x,(s1, s2)):xs) cond = case s1 of
                        Nothing    -> case s2 of
                            Nothing    -> (x, If cond Skip Skip) : wrapIf xs cond --fragwürdig?
                            Just stmt2 -> (x, If cond Skip stmt2) : wrapIf xs cond
                        Just stmt1 -> case s2 of
                            Nothing    -> (x, If cond stmt1 Skip) : wrapIf xs cond
                            Just stmt2 -> (x, If cond stmt1 stmt2) : wrapIf xs cond
            For expr s1 ->
                let grouped = foldl insertStmt (ChannelBehavior []) (helper s1)
                in wrapFor grouped expr where
                    wrapFor :: ChannelBehavior -> Expr -> [(Channel, Statement)]
                    wrapFor (ChannelBehavior xs) expr =
                       [(c, For expr stmt) | (c, stmt) <- xs]
            -- TODO
            Assign var val -> []
            Go s1 s2 -> []
            New channel s -> []