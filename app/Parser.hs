module Parser where
import Datastructure
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as Lex
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String
-- whitespaces werden "geschluckt"
spaceConsumer :: Parser ()
spaceConsumer = do
  let spaceOrCommentOrIgnore =
        space1
        <|> skipLineComment "//"
        <|> skipBlockComment "/*" "*/"
        <|> try ignoreParser
  many spaceOrCommentOrIgnore
  return ()

-- Unterstriche in Go überall erlaubt bei identifieren, auch am Anfang
identifier :: Parser Channel
identifier = do
    a <- lowerChar <|> char '_'
    b <- many (alphaNumChar <|> char '_')
    return (a:b)

-- Hilfsparser der jede mögliche Zahl als String parst
numberParser :: Parser String
numberParser = try parseInt <|> parseFloat where
    parseInt = fmap show (Lex.signed spaceConsumer Lex.decimal)
    parseFloat = fmap show (Lex.signed spaceConsumer Lex.float)

-- Hilfsparser atomare Ausdrücke (Bool, Identifier, Recv, Literale)
atom :: Parser String
atom = try boolParser <|> try recvParser <|> try numberParser <|> identifier

boolParser :: Parser String
boolParser = do
    spaceConsumer
    b <- string "false" <|> string "true"
    spaceConsumer
    return b
recvParser :: Parser String
recvParser = do
    spaceConsumer
    _ <- string "<-"
    spaceConsumer
    c <- identifier
    return ("<-" ++ c)

-- Hilfsparser für Operatoren
operator :: Parser (String -> String -> String)
operator = do
    spaceConsumer
    op <- choice $ map string ["+", "-", "*", "/", "%", ">=", "<=", "==", "!=", ">", "<", "&&"]
    spaceConsumer
    return (\a b -> a ++ op ++ b)

-- momentan keine geklammerten ausdrücke erlaubt
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
    spaceConsumer
    x <- p
    spaceConsumer
    rest x
    where
    rest x = (do
        f <- op
        spaceConsumer
        y <- p
        spaceConsumer
        rest (f x y)) <|> return x

expressionParser :: Parser Expr
expressionParser = chainl1 atom operator

-- Ausdrücke wie i := 0 sind zu ignorieren
ignoreParser :: Parser ()
ignoreParser = do
    _ <- identifier
    spaceConsumer
    _ <- string ":="
    spaceConsumer
    _ <- identifier <|> numberParser <|> expressionParser
    spaceConsumer
    return ()

-- kein Parser für Sequence, für Sequence siehe parseStatement
-- ignoriert alle Ausdrücke, die keinem Statement zugeordnet werden können
parseSingleStatement :: Parser Statement
parseSingleStatement = do
    try parseEnd <|> try parseRec <|> try parseSend <|> try parseSkip <|> try parseFor <|> try parseIf

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
    _ <- try expressionParser <|> try identifier <|> try numberParser
    return (Send c)

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
    Receive <$> identifier

parseEnd :: Parser Statement
parseEnd = do
    spaceConsumer
    _ <- string "close"
    spaceConsumer
    End <$> identifier

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
  stmts <- parseSingleStatement `sepEndBy1` (spaceConsumer <|> ignoreParser)
  return $ foldr1 Sequence stmts

