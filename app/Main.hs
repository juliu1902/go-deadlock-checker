{-# LANGUAGE InstanceSigs #-}

module Main where

import qualified Data.Map           as Map (empty, fromList, lookup, toList, union, filterWithKey)
import           Datastructure
import           Parser
import           System.Environment (getArgs)
import           System.Exit        (die)
import           Text.Megaparsec
import           AlternativeST
import           Equivalence
import           Duality
import           Substitution
import Data.SBV.Tuple (_1)

-- Parses a program and returns its Session Type or an error if it couldn't be parsed
buildST :: Function -> IO (Either String (Statement, Context))
buildST (Function name decs stmt) = do
  st <- stmtToST Map.empty (initialContext decs) 0 stmt 
  case st of
    Left err         -> return (Left $ "Fehler bei Typprüfung: " ++ err)
    Right (_, s, ctxt, state) -> return (Right (s, ctxt))

buildAlternativeST :: Function -> IO (Statement, Context)
buildAlternativeST (Function name decs stmt) = do
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


main :: IO ()
main = do
  args <- getArgs
  path <-
    case args of
      (p:_) -> pure p
      []    -> pure "inputs.txt"
  content <- readFile path
  let blocks = filter (not . all (`elem` "\n\t ")) (splitAllByLine content) -- spaces are ignored
  mapM_ (uncurry runCase) (zip [1 ..] blocks)

-- statement Variablen c1 -> a, c2 -> b wenn func foo (c1,c2) aufgerufen wird mit foo(a,b)
evaluateFunctionCalls :: Function -> Context -> Functioncall -> IO Statement
evaluateFunctionCalls (Function funcName varDecs stmt) ctxt (Functioncall callName callVarDecs) = 
  if callName == funcName then
    let abbildung = zip [name |(name, vtype) <- varDecs] callVarDecs
        substitutionMap = Map.fromList abbildung
    in return (substituteVars substitutionMap stmt)
  else 
    error $ "Function call name " ++ show callName ++ " does not match function name " ++ show funcName

runCase :: Int -> String -> IO ()
runCase i input = do
  case runParser parseInput "" input of
    Left err -> die ("Fehler beim Parsen: " ++ errorBundlePretty err)
    Right (Function func1 varDecs1 srcA, Function func2 varDecs2 srcB, args, Functioncall call1 varCall1, Functioncall call2 varCall2) -> do
      let args' = initialContext args
      stmt1 <- evaluateFunctionCalls (Function func1 varDecs1 srcA) args' (Functioncall call1 varCall1)
      stmt2 <- evaluateFunctionCalls (Function func2 varDecs2 srcB) args' (Functioncall call2 varCall2)
      let filteredContext1 = Map.filterWithKey (\k _ -> k `elem` varCall1) args'
      let filteredContext2 = Map.filterWithKey (\k _ -> k `elem` varCall2) args'
      let filteredArgs1 = [(a, t) | (a, (t, av)) <- Map.toList filteredContext1]
      let filteredArgs2 = [(a, t) | (a, (t, av)) <- Map.toList filteredContext2]
      st1 <- stmtToST Map.empty filteredContext1 0 stmt1 
      st2 <- stmtToST Map.empty filteredContext2 0 stmt2
      -- Create Function objects for buildAlternativeST
      let func1WithFilteredArgs = Function func1 filteredArgs1 stmt1
      let func2WithFilteredArgs = Function func2 filteredArgs2 stmt2
      (alternativeST1, paramContext1) <- buildAlternativeST func1WithFilteredArgs
      (alternativeST2, paramContext2) <- buildAlternativeST func2WithFilteredArgs
      case (st1, st2) of
        (Left e, _) -> die $ "Fehler in Block " ++ show i ++ "A:\n" ++ e
        (_, Left e) -> die $ "Fehler in Block " ++ show i ++ "B:\n" ++ e
        (Right (_, stA, ctA, _), Right (_, stB, ctB, _)) -> do
          putStrLn $ "========== Paar " ++ show i ++ " =========="
          case checkClosed stA of
            Left err -> putStrLn $ "Fehler in Block " ++ show i ++ "A: " ++ err
            Right () -> return ()
          case checkClosed stB of
            Left err -> putStrLn $ "Fehler in Block " ++ show i ++ "B: " ++ err
            Right () -> return ()
          putStrLn ("main context: " ++ show args')
          putStrLn "ST A:"
          putStrLn (prettyPrintST stA)
          putStrLn (prettyPrintST alternativeST1)
          putStrLn "\nST B:"
          putStrLn (prettyPrintST stB)
          putStrLn (prettyPrintST alternativeST2)

          putStrLn $ "\nContext A: " ++ show ctA
          putStrLn $ "Context B: " ++ show ctB
          putStrLn "\nTests:"
          resEquiv <- testEquivalence' paramContext1 paramContext2 [] (addSkip stA) (addSkip stB)
          putStrLn ("A ~ B? " ++  show resEquiv)
          resDual <- testDuality paramContext1 paramContext2 [] [] [] (addSkip stA) (addSkip stB)
          putStrLn ("A ^ B? " ++ show resDual)