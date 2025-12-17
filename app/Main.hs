{-# LANGUAGE InstanceSigs #-}

module Main where

import qualified Data.Map           as Map (empty, fromList, lookup, toList, union)
import           Datastructure
import           Parser
import           System.Environment (getArgs)
import           System.Exit        (die)
import           Text.Megaparsec
import           AlternativeST

testEquiv :: Statement -> Statement -> Bool
testEquiv s1 s2 =
  testEquivalence
    (canonicalizeChannelNames (normalizeST s1) Map.empty)
    (canonicalizeChannelNames (normalizeST s2) Map.empty)

testDual :: Statement -> Statement -> Bool
testDual s1 s2 = testEquiv (dual s1) s2

-- Parses a program and returns its Session Type or an error if it couldn't be parsed
buildST' :: String -> IO (Either String (Statement, Context))
buildST' src =
  case runParser parseProgram "" src of
    Left err ->
      return (Left $ "Fehler beim Parsen: " ++ (errorBundlePretty err))
    Right (Program decs stmt) -> do
      st <- stmtToST (initialContext decs) 0 stmt 
      case st of
        Left err         -> return (Left $ "Fehler bei Typprüfung: " ++ err)
        Right (st, ctxt, state) -> return (Right (st, ctxt))

buildAlternativeST :: String -> IO (Statement, Context)
buildAlternativeST src =
  case runParser parseProgram "" src of
    Left err -> return (Skip, Map.empty)
    Right (Program decs stmt) -> alternativeSTNaming (initialContext decs) stmt


checkClosed :: Statement -> Either String ()
checkClosed st =
  case checkClosedHelper st [] of
    Right closedVars -> Right ()
    Left err         -> Left err
  where
    checkClosedHelper :: Statement -> [VarName] -> Either String [VarName]
    checkClosedHelper st closedVars =
      case st of
        Send v ->
          if v `elem` closedVars
            then Left "Send on closed channel."
            else Right closedVars
        Receive v ->
          if v `elem` closedVars
            then Left "Receive on closed channel."
            else Right closedVars
        End v -> Right (closedVars ++ [v])
        Sequence s1 s2 -> do
          firstClosed <- checkClosedHelper s1 closedVars
          checkClosedHelper s2 firstClosed
        If e s1 s2 -> do
          case ( checkClosedHelper s1 closedVars
               , checkClosedHelper s2 closedVars) of
            (Right fvars, Right svars) -> Right (fvars ++ svars)
            _ -> Left "Send/Receive on closed channel in If branch"
        For fh s -> checkClosedHelper s closedVars
        _ -> Right closedVars


-- creates a list of strings out of a long string where --- stands for a split
splitAllByLine :: String -> [String]
splitAllByLine x = helper (lines x)
  where
    helper :: [String] -> [String]
    helper [] = [""]
    helper (l:ls) =
      if l == "---"
        then [""] ++ helper ls
        else case (helper ls) of
               (y:ys) -> [(unlines ([l] ++ lines y))] ++ ys
               _      -> [l]


-- make program blocks into pairs
toPairs :: [a] -> Either String [(a, a)]
toPairs (a:b:rest) = do
  pairs <- toPairs rest
  return ((a, b) : pairs)
toPairs [] = Right []
toPairs [_] =
  Left "Ungerade Anzahl von Programmblöcken: jeder Test braucht zwei Blöcke."

main :: IO ()
main = do
  args <- getArgs
  path <-
    case args of
      (p:_) -> pure p
      []    -> pure "inputs.txt"
  content <- readFile path
  let blocks = filter (not . all (`elem` "\n\t ")) (splitAllByLine content) -- spaces are ignored
  case toPairs blocks of
    Left e   -> die e
    Right ps -> mapM_ (uncurry runCase) (zip [1 ..] ps)

runCase :: Int -> (String, String) -> IO ()
runCase i (srcA, srcB) = do
  st1 <- buildST' srcA
  st2 <- buildST' srcB
  alternativeST1 <- buildAlternativeST srcA
  alternativeST2 <- buildAlternativeST srcB
  case (st1, st2) of
    (Left e, _) -> die $ "Fehler in Block " ++ show i ++ "A:\n" ++ e
    (_, Left e) -> die $ "Fehler in Block " ++ show i ++ "B:\n" ++ e
    (Right (stA, ctA), Right (stB, ctB)) -> do
      putStrLn $ "========== Paar " ++ show i ++ " =========="
      case checkClosed stA of
        Left err -> putStrLn $ "Fehler in Block " ++ show i ++ "A: " ++ err
        Right () -> return ()
      case checkClosed stB of
        Left err -> putStrLn $ "Fehler in Block " ++ show i ++ "B: " ++ err
        Right () -> return ()
      putStrLn "ST A:"
      putStrLn (prettyPrintST stA)
      putStrLn (prettyPrintST (fst alternativeST1))
      putStrLn "\nST B:"
      putStrLn (prettyPrintST stB)
      putStrLn (prettyPrintST (fst alternativeST2))

      putStrLn "\nNormalformen:"
      putStrLn ("A: " ++ prettyPrintST (normalizeST stA))
      putStrLn ("B: " ++ prettyPrintST (normalizeST stB))

      putStrLn $ "\nContext A: " ++ show ctA
      putStrLn $ "Context B: " ++ show ctB
      putStrLn "\nTests:"
      putStrLn ("OLD A ~ B?        " ++ show (testEquiv stA stB))
      putStrLn ("OLD dual (A) ~ B?  " ++ show (testDual stA stB))
      resEquiv <- (testEquivalence' (Map.union ctA ctB) [] (canonicalizeChannelNames (normalizeST stA) Map.empty) (canonicalizeChannelNames (normalizeST stB) Map.empty))
      putStrLn ("A ~ B?" ++  show resEquiv)
      putStrLn ""
