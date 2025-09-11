module Parser where
import Datastructure
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as Lex
import Data.Void
import Text.Megaparsec
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String
-- whitespaces werden "geschluckt"
spaceConsumer :: Parser ()
spaceConsumer = do
  let spaceOrCommentOrIgnore =
        space1
        <|> skipLineComment "//"
        <|> skipBlockComment "/*" "*/"
  many spaceOrCommentOrIgnore
  return ()

-- hilfsparser, der ein symbol parst und die whitespaces davor und danach wegschmeißt
singleSymbol :: String -> Parser String
singleSymbol s = do
    spaceConsumer
    sym <- string s
    spaceConsumer
    return sym

-- Unterstriche in Go überall erlaubt bei identifieren, auch am Anfang
identifier :: Parser String
identifier = do
    a <- lowerChar <|> char '_'
    b <- many (alphaNumChar <|> char '_')
    spaceConsumer
    return (a:b)
-- ein identifier könnte entweder eine variable oder ein channel sein
parseVar :: Parser VarName
parseVar = VarName <$> identifier

parseChan :: Parser ChannelID
parseChan = ChannelID <$> identifier

-- Ausdrücke wie i := 0 sind zu ignorieren
parseAssign :: Parser Statement
parseAssign = do
    i <- parseVar
    spaceConsumer
    _ <- string ":="
    spaceConsumer
    _ <- (EVar <$> parseVar)<|> numberParser <|> expressionParser
    spaceConsumer
    return $ Assign i Aunknown

-- Hilfsparser der jede mögliche Zahl als String parst
numberParser :: Parser Expr
numberParser = try parseFloat <|> parseInt where
    parseInt = EInt <$> Lex.lexeme spaceConsumer Lex.decimal
    parseFloat = EFloat <$> Lex.lexeme spaceConsumer Lex.float

boolParser :: Parser Expr
boolParser = do
    spaceConsumer
    b <- string "true" >> return True <|> (string "false" >> return False)
    spaceConsumer
    return $ EBool b

parseVarTypes :: Parser VarType
parseVarTypes = do
    spaceConsumer
    x <- try parseChan <|> try parseBool <|> parseInt
    return x
  where
    parseInt = do
        _ <- string "int"
        return TInt
    parseBool = do
        _ <- string "bool"
        return TBool
    parseChan = do
        _ <- string "chan"
        spaceConsumer
        y <- parseCInt <|> parseCBool
        return (TChan y)
      where
        parseCInt = do
            _ <- string "int"
            return CInt
        parseCBool = do
            _ <- string "bool"
            return CBool

varDecParser :: Parser VarDec 
varDecParser = do 
    spaceConsumer 
    _ <- string "var" 
    spaceConsumer 
    v <- identifier
    ty <- parseVarTypes
    return (v, ty)

parseVarDecs :: Parser VarDecs
parseVarDecs = do
    varDecParser `sepEndBy` spaceConsumer

-- ( ... )-Ausdrücke
parensExpr :: Parser Expr
parensExpr = do
    spaceConsumer
    _ <- char '('
    e <- expressionParser
    spaceConsumer
    _ <- char ')'
    pure e

-- "Term": Werte, Variablen oder geklammert
term :: Parser Expr
term =  try boolParser
    <|> try numberParser
    <|> EVar <$> try parseVar
    <|> parensExpr

-- Operator table für die makeExprParser funktion
table :: [[Operator Parser Expr]]
table =
  [ 
    [Prefix (singleSymbol "-" >> return (EBinOp Sub (EInt 0))) ], -- highest precedence

    [InfixL (singleSymbol "*" >> return (EBinOp Mul)), 
    InfixL (singleSymbol "/" >> return (EBinOp Div)),
    InfixL (singleSymbol "%" >> return (EBinOp Mod))],

    [InfixL (singleSymbol "+" >> return (EBinOp Add)), 
    InfixL (singleSymbol "-" >> return (EBinOp Sub))],

    [InfixN (singleSymbol ">=" >> return (EBinOp Ge)),
    InfixN (singleSymbol "<=" >> return (EBinOp Le)), 
    InfixN (singleSymbol ">" >>  return (EBinOp Gt)), 
    InfixN (singleSymbol "<" >>  return (EBinOp Lt))], 

    [InfixN (singleSymbol "==" >> return (EBinOp Eq)), 
    InfixN (singleSymbol "!=" >> return (EBinOp Neq))], 

    [InfixL (singleSymbol "&&" >> return (EBinOp And)), 
    InfixL (singleSymbol "||" >> return (EBinOp Or)) ] -- lowest precedence
  ]

expressionParser :: Parser Expr
expressionParser = makeExprParser term table

parseSingleStatement :: Parser Statement
parseSingleStatement = do
    try parseMakeBlock <|> try parseAssign <|> try parseEnd <|> try parseRec <|> try parseSend <|> try parseSkip <|> try parseFor <|> parseIf

parseMakeChanName :: Parser String
parseMakeChanName = do
    spaceConsumer
    c <- identifier
    spaceConsumer
    _ <- string "::="
    spaceConsumer
    _ <- string "make"
    spaceConsumer
    _ <- char '('
    spaceConsumer
    _ <- string "chan"
    spaceConsumer
    _ <- string "int" <|> string "bool"
    spaceConsumer
    _ <- char ')'
    return c

parseMakeBlock :: Parser Statement
parseMakeBlock = do
    c <- parseMakeChanName                 -- c ::= make(chan int|bool)
    spaceConsumer
    s <- parseStatement
    return (New (ChannelID c) s)

parseSkip :: Parser Statement
parseSkip = do
    spaceConsumer
    _ <- string "skip"
    return Skip

-- akzeptiert jede beliebige Zahl, gültige Variablennamen und einfache Operationen wie 2*x
parseSend :: Parser Statement
parseSend = do
    spaceConsumer
    c <- identifier
    spaceConsumer
    _ <- string "<-"
    spaceConsumer
    _ <- try expressionParser <|> try term <|> numberParser
    return (Send (ChannelID c))

-- sowohl x = <- c als auch x := <- c erlaubt, 
-- obwohl bei x = <- c x vorher deklariert werden muss 
-- mit `var x int` oder ähnlichem, hier nicht berücksichtigt
parseRec :: Parser Statement
parseRec = do
    spaceConsumer
    _ <- identifier
    spaceConsumer
    _ <- optional (char ':')
    spaceConsumer
    _ <- char '='
    spaceConsumer
    _ <- string "<-"
    spaceConsumer
    Receive <$> parseChan

parseEnd :: Parser Statement
parseEnd = do
    spaceConsumer
    _ <- string "close"
    spaceConsumer
    End <$> parseChan

-- Bis jetzt nur einfache comparison expressions erlaubt
parseIf :: Parser Statement
parseIf = do
    spaceConsumer
    _ <- string "if"
    spaceConsumer
    cond <- expressionParser
    spaceConsumer
    _ <- string "then"
    spaceConsumer
    a <- parseSequence <|> parseSingleStatement
    spaceConsumer
    _ <- string "else"
    spaceConsumer
    b <- parseSequence <|> parseSingleStatement
    return (If cond a b)

parseFor :: Parser Statement
parseFor = do
    spaceConsumer
    _ <- string "for"
    spaceConsumer
    cond <- expressionParser
    s <- parseSequence
    return (For cond s)

-- used for {}-codeblocks inside for/ifs
parseSequence :: Parser Statement
parseSequence = do
    spaceConsumer
    _ <- char '{'
    spaceConsumer
    s <- parseStatement
    spaceConsumer
    _ <- char '}'
    return s

-- Sequence Parser hier indirekt verbaut
parseStatement :: Parser Statement
parseStatement = do
    stmts <- parseSingleStatement `sepEndBy1` spaceConsumer
    return $ foldr1 Sequence stmts

parseProgram :: Parser Program
parseProgram = do
    decs <- parseVarDecs
    stmt <- parseStatement
    return (Program decs stmt)


