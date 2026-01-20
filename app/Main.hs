{-# LANGUAGE InstanceSigs #-}

module Main where

import qualified Data.Map           as Map (empty, fromList, lookup, toList, union)
import           Datastructure
import           Parser
import           System.Environment (getArgs)
import           System.Exit        (die)
import           Text.Megaparsec
import           AlternativeST
import           Equivalence
import           Duality
import Data.SBV.Tuple (_1)

-- Parses a program and returns its Session Type or an error if it couldn't be parsed
buildST' :: String -> IO (Either String (Statement, Context))
buildST' src =
  case runParser parseProgram "" src of
    Left err ->
      return (Left $ "Fehler beim Parsen: " ++ (errorBundlePretty err))
    Right (Program decs stmt) -> do
      st <- stmtToST Map.empty (initialContext decs) 0 stmt 
      case st of
        Left err         -> return (Left $ "Fehler bei Typprüfung: " ++ err)
        Right (_, s, ctxt, state) -> return (Right (s, ctxt))

buildAlternativeST :: String -> IO (Statement, Context)
buildAlternativeST src =
  case runParser parseProgram "" src of
    Left _ -> return (Skip, Map.empty)
    Right (Program decs stmt) -> do
      (resST, _, _, _) <- alternativeSTNaming Map.empty (initialContext decs) 0 stmt
      return (resST, initialContext decs)


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
        End v -> Right (closedVars ++ [v])
        Sequence s1 s2 -> do
          firstClosed <- checkClosedHelper s1 closedVars
          checkClosedHelper s2 firstClosed
        If e s1 s2 -> do
          case ( checkClosedHelper s1 closedVars
               , checkClosedHelper s2 closedVars) of
            (Right fvars, Right svars) -> Right (fvars ++ svars)
            _ -> Left "Send on closed channel in If branch"
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
  (alternativeST1, paramContext1) <- buildAlternativeST srcA
  (alternativeST2, paramContext2) <- buildAlternativeST srcB
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
      putStrLn (prettyPrintST alternativeST1)
      putStrLn "\nST B:"
      putStrLn (prettyPrintST stB)
      putStrLn (prettyPrintST alternativeST2)

      putStrLn "\nNormalform mit kanonisierter Variablenbenennung"
      putStrLn
        ("Canonized A: "
           ++ prettyPrintST
                (canonicalizeChannelNames (simplification stA) Map.empty))
      putStrLn
        ("Canonized B: "
           ++ prettyPrintST
                (canonicalizeChannelNames (simplification stB) Map.empty))
      --putStrLn $ "\nContext A: " ++ show ctA
      --putStrLn $ "Context B: " ++ show ctB
      putStrLn "\nTests:"
      resEquiv <- testEquivalence' paramContext1 paramContext2 [] (addSkip (canonicalizeChannelNames stA Map.empty)) (addSkip (canonicalizeChannelNames stB Map.empty))
      putStrLn ("A ~ B? " ++  show resEquiv)
      resDual <- testDuality paramContext1 paramContext2 [] [] [] (addSkip (canonicalizeChannelNames stA Map.empty)) (addSkip (canonicalizeChannelNames stB Map.empty))
      putStrLn ("A ^ B? " ++ show resDual)
      --putStrLn $ show stA
      --print $ show (simplification stA)
      print (addSkip (canonicalizeChannelNames stA Map.empty))
      print $ prettyPrintST (addSkip (canonicalizeChannelNames stA Map.empty))
      print (addSkip (canonicalizeChannelNames stB Map.empty))
      --print $ prettyPrintST ((canonicalizeChannelNames stA Map.empty))
      print $ prettyPrintST (addSkip (canonicalizeChannelNames stB Map.empty))
      --print $ prettyPrintST (simplification (addSkip (canonicalizeChannelNames stB Map.empty)))