module Parser where

import           Datastructure
import           Text.Megaparsec.Char as C
import           Text.Megaparsec.Char.Lexer as Lex
import           Control.Monad.Combinators.Expr
import           Data.Void
import           Text.Megaparsec
import           Control.Monad.Combinators.Expr
import qualified Data.Map            as Map
type Parser = Parsec Void String


-- whitespaces werden "geschluckt"
spaceConsumer :: Parser ()
spaceConsumer = do
  let spaceOrCommentOrIgnore =
        space1 <|> skipLineComment "//" <|> skipBlockComment "/*" "*/"
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
  return (a : b)


-- ein identifier könnte entweder eine variable oder ein channel sein
parseVar :: Parser VarName
parseVar = VarName <$> identifier


-- Ausdrücke wie i = 0 oder c = c1
parseAssign :: Parser Statement
parseAssign = do
  i <- parseVar
  spaceConsumer
  _ <- string "="
  spaceConsumer
  expr <- expressionParser
  spaceConsumer
  return $ Assign i expr


-- Hilfsparser der jede mögliche Zahl als String parst
numberParser :: Parser Expr
numberParser = try parseFloat <|> parseInt
  where
    parseInt   = EInt <$> Lex.lexeme spaceConsumer Lex.decimal
    parseFloat = EFloat <$> Lex.lexeme spaceConsumer Lex.float

boolParser :: Parser Expr
boolParser = do
  spaceConsumer
  b <- try (string "true" >> return True) <|> (string "false" >> return False)
  spaceConsumer
  return $ EBool b

parseVarTypes :: Parser VarType
parseVarTypes = do
  spaceConsumer
  x <- try parseVar <|> try parseBool <|> parseInt
  return x
  where
    parseInt = do
      _ <- string "int"
      return TInt
    parseBool = do
      _ <- string "bool"
      return TBool
    parseVar = do
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


varDeclareParser :: Parser Statement
varDeclareParser = do
  spaceConsumer
  _ <- string "var"
  spaceConsumer
  v <- identifier
  ty <- parseVarTypes
  return $ Declare (VarName v) ty

parameterParser :: Parser VarDec
parameterParser = do
  spaceConsumer
  v <- identifier
  ty <- parseVarTypes
  return (VarName v, ty)

headerParser :: Parser VarDecs
headerParser = do
  spaceConsumer
  _ <- string "func"
  spaceConsumer
  _ <- identifier
  spaceConsumer
  _ <- char '('
  decs <- parameterParser `sepBy` (spaceConsumer *> char ',' <* spaceConsumer)
  spaceConsumer
  _ <- char ')'
  return decs

funcParser :: Parser Statement
funcParser = do
  spaceConsumer
  _ <- string "func"
  spaceConsumer
  name <- identifier
  spaceConsumer
  _ <- char '('
  decs <- parameterParser `sepBy` (spaceConsumer *> char ',' <* spaceConsumer)
  spaceConsumer
  _ <- char ')'
  spaceConsumer
  _ <- char '{'
  stmt <- parseStatement
  _ <- char '}'
  return (Func (VarName name) decs stmt)

goParser :: Parser Statement
goParser = do
  spaceConsumer
  _ <- string "go"
  spaceConsumer
  name <- identifier
  spaceConsumer
  _ <- char '('
  spaceConsumer
  args <- parseVar `sepBy` (spaceConsumer *> char ',' <* spaceConsumer)
  _ <- char ')'
  return (GoCall (VarName name) args)




-- ( ... )-Ausdrücke
parensExpr :: Parser Expr
parensExpr = do
  spaceConsumer
  _ <- char '('
  spaceConsumer
  e <- expressionParser
  spaceConsumer
  _ <- char ')'
  spaceConsumer
  pure e

-- "Term": Werte, Variablen oder geklammert
term :: Parser Expr
term =
  try boolParser <|> try numberParser <|> EVar <$> try parseVar <|> parensExpr


-- Operator table für die makeExprParser funktion
table :: [[Operator Parser Expr]]
table =
  [ [ Prefix (singleSymbol "!" >> return ENot )
    , Prefix (singleSymbol "-" >> return (EBinOp Sub (EInt 0)))
    ] -- highest precedence
  , [ InfixL (singleSymbol "*" >> return (EBinOp Mul))
    , InfixL (singleSymbol "/" >> return (EBinOp Div))
    , InfixL (singleSymbol "%" >> return (EBinOp Mod))
    ]
  , [ InfixL (singleSymbol "+" >> return (EBinOp Add))
    , InfixL (singleSymbol "-" >> return (EBinOp Sub))
    ]
  , [ InfixN (singleSymbol ">=" >> return (EBinOp Ge))
    , InfixN (singleSymbol "<=" >> return (EBinOp Le))
    , InfixN (singleSymbol ">" >> return (EBinOp Gt))
    , InfixN (singleSymbol "<" >> return (EBinOp Lt))
    ]
  , [ InfixN (singleSymbol "==" >> return (EBinOp Eq))
    , InfixN (singleSymbol "!=" >> return (EBinOp Neq))
    ]
  , [ InfixL (singleSymbol "&&" >> return (EBinOp And))
    , InfixL (singleSymbol "||" >> return (EBinOp Or)) -- lowest precedence
    ]
  ]

expressionParser :: Parser Expr
expressionParser = makeExprParser term table

parseSingleStatement :: Parser Statement
parseSingleStatement = do
  try parseAssign
    <|> try parseEnd
    <|> try parseRec
    <|> try parseSend
    <|> try parseSkip
    <|> try parseFor
    <|> try varDeclareParser
    <|> try goParser
    <|> try funcParser
    <|> parseIf

parseMake :: Parser Statement
parseMake = do
  spaceConsumer
  c <- identifier
  spaceConsumer
  _ <- string "="
  spaceConsumer
  _ <- string "make"
  spaceConsumer
  _ <- char '('
  spaceConsumer
  _ <- string "chan"
  spaceConsumer
  chanType <- try (string "int" >> return CInt) <|> (string "bool" >> return CBool)
  spaceConsumer
  _ <- char ')'
  spaceConsumer
  stmt <- try parseSequence <|> parseStatement
  return $ Make (VarName c) chanType stmt


--parseMakeBlock :: Parser Statement
--parseMakeBlock = do
--  c <- parseMakeChanName -- c ::= make(chan int|bool)
--  spaceConsumer
--  s <- parseStatement
--  return (New (VarName c) s)

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
  _ <- term  -- Use term instead of full expressionParser to avoid over-consumption
  spaceConsumer
  return (Send (VarName c))

parseRec :: Parser Statement
parseRec = do
  try recAndThrowParser <|> recParser

-- <- c auch erlaubt, schmeißt den wert der in c steckt weg
recAndThrowParser :: Parser Statement
recAndThrowParser = do
  spaceConsumer
  _ <- string "<-"
  spaceConsumer
  Receive <$> parseVar

-- sowohl x = <- c als auch x := <- c erlaubt
recParser :: Parser Statement
recParser = do 
  spaceConsumer
  _ <- identifier
  spaceConsumer
  _ <- char '='
  spaceConsumer
  _ <- string "<-"
  spaceConsumer
  Receive <$> parseVar

parseEnd :: Parser Statement
parseEnd = do
  spaceConsumer
  _ <- string "close"
  spaceConsumer
  End <$> parseVar

parseCondPlaceHolder :: Parser Expr
parseCondPlaceHolder = do
  spaceConsumer
  _ <- char '*'
  spaceConsumer
  return $ EVar (VarName "*")


-- Bis jetzt nur einfache comparison expressions erlaubt
parseIf :: Parser Statement
parseIf = do
  spaceConsumer
  _ <- string "if"
  spaceConsumer
  cond <- try expressionParser <|> parseCondPlaceHolder
  spaceConsumer
  _ <- string "then"
  spaceConsumer
  a <- try parseSequence <|> parseSingleStatement
  spaceConsumer
  _ <- string "else"
  spaceConsumer
  b <- try parseSequence <|> parseSingleStatement
  return (If cond a b)


-- For ForHeader Statement
parseFor :: Parser Statement
parseFor = do
  spaceConsumer
  head <- parseForHeader
  spaceConsumer
  s <- parseSequence
  return (For head s)

parseForHeader :: Parser ForHeader
parseForHeader = do
  spaceConsumer
  _ <- string "for"
  spaceConsumer
  _ <- char '('
  spaceConsumer
  x <- identifier
  spaceConsumer
  try (parseRunning x) <|> parseRange x
  where
    parseRunning :: String -> Parser ForHeader
    parseRunning x = do
      _ <- char '='
      spaceConsumer
      start <- Lex.lexeme spaceConsumer Lex.decimal
      spaceConsumer
      _ <- char ';'
      spaceConsumer
      e <- expressionParser
      spaceConsumer
      _ <- char ';'
      spaceConsumer
      incdec <- parseIncDec
      spaceConsumer
      _ <- char ')'
      return $ ForHeaderRunning (VarName x) start e incdec
    parseRange :: String -> Parser ForHeader
    parseRange x = do
      _ <- string ":="
      spaceConsumer
      __ <- string "range"
      spaceConsumer
      chan <- identifier
      spaceConsumer
      _ <- char ')'
      return $ ForHeaderRange (VarName x) (VarName chan)

parseIncDec :: Parser IncDec
parseIncDec = do
  _ <- identifier
  try parseInc <|> parseDec
  where
    parseInc = do
      _ <- string "++"
      return Inc
    parseDec = do
      _ <- string "--"
      return Dec


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
  stmts <- (try parseMake <|> parseSingleStatement) `sepEndBy1` spaceConsumer
  return $ foldr1 Sequence stmts


parseStatement' :: Parser Statement
parseStatement' = do
  statements <- statementList
  return $ case statements of
    []  -> Skip
    [s] -> s
    _   -> foldr1 Sequence statements
  where
    statementList = do
      spaceConsumer
      first <- try parseMake <|> parseSingleStatement
      rest <- many (try parseNextStatement)
      return (first : rest)
    parseNextStatement = do
      spaceConsumer
      -- Explicitly check we're not at the end
      notFollowedBy (char '}')
      stmt <- try parseMake <|> parseSingleStatement  
      return stmt

parseFunction :: Parser Function
parseFunction = do
  spaceConsumer
  _ <- string "func"
  spaceConsumer
  x <- identifier
  spaceConsumer
  _ <- char '('
  decs <- parameterParser `sepBy` (spaceConsumer *> char ',' <* spaceConsumer)
  _ <- char ')'
  spaceConsumer
  _ <- char '{'
  spaceConsumer
  stmt <- parseStatement
  spaceConsumer
  _ <- char '}'
  return (Function (VarName x) decs stmt)

varDecParser :: Parser VarDec
varDecParser = do
  spaceConsumer
  _ <- string "var"
  spaceConsumer
  v <- identifier
  ty <- parseVarTypes
  return (VarName v, ty)


parseInput :: Parser (Function, Function, VarDecs, Functioncall, Functioncall)
parseInput = do
  spaceConsumer
  a <- parseFunction
  spaceConsumer
  b <- parseFunction
  spaceConsumer
  vardecs <- varDecParser `sepEndBy1` spaceConsumer
  spaceConsumer
  name1 <- identifier
  _ <- char '('
  args1 <- parseVar `sepBy` (spaceConsumer *> char ',' <* spaceConsumer)
  spaceConsumer
  _ <- char ')'
  spaceConsumer
  name2 <- identifier
  _ <- char '('
  args2 <- parseVar `sepBy` (spaceConsumer *> char ',' <* spaceConsumer)
  spaceConsumer
  _ <- char ')'
  spaceConsumer
  return (a, b, vardecs, Functioncall (VarName name1) args1, Functioncall (VarName name2) args2)