{-# LANGUAGE InstanceSigs #-}

module Main where

import qualified Data.Map           as Map (empty, fromList, lookup, toList, union, filterWithKey)
import           Datastructure
import           Parser
import           System.Environment (getArgs)
import           System.Exit        (die)
import           Text.Megaparsec
import           Equivalence
import           Duality
import           Substitution

checkClosed :: Statement -> Either String ()
checkClosed st =
  case checkClosedHelper st [] of
    Right _ -> Right ()
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
        _ -> Right closedVars


-- Split input by "---"
splitBlocks :: String -> [String]
splitBlocks content = 
  let blocks = map unlines . split [] . lines $ content
  in filter (not . all (`elem` " \n\t")) blocks
  where
    split acc [] = [reverse acc]
    split acc (line:rest)
      | line == "---" = reverse acc : split [] rest
      | otherwise = split (line:acc) rest

main :: IO ()
main = do
  args <- getArgs
  let path = if null args then "inputs.txt" else head args
  content <- readFile path
  let blocks = splitBlocks content
  mapM_ (uncurry runCase) (zip [1..] blocks)

-- Substitute function parameters with actual arguments names
substituteFunctionCall :: Function -> Functioncall -> Statement
substituteFunctionCall (Function funcName params stmt) (Functioncall callName args) 
  | callName == funcName = 
    let substitutions = zip (map fst params) args
        substitutionMap = Map.fromList substitutions
    in substituteVars substitutionMap stmt
  | otherwise = error $ "Function name mismatch: " ++ show callName ++ " vs " ++ show funcName

runCase :: Int -> String -> IO ()
runCase i input = do
  case runParser parseInput "" input of
    Left err -> die ("Fehler beim Parsen: " ++ errorBundlePretty err)
    Right (func1, func2, args, call1@(Functioncall _ varCall1), call2@(Functioncall _ varCall2)) -> do
      let args' = initialContext args
          stmt1 = substituteFunctionCall func1 call1
          stmt2 = substituteFunctionCall func2 call2

      -- Filter context and varDecs to include variables that are only used in each function call
      let filteredContext1 = Map.filterWithKey (\k _ -> k `elem` varCall1) args'
          filteredContext2 = Map.filterWithKey (\k _ -> k `elem` varCall2) args'
          filteredArgs1 = [(a, t) | (a, (t, av)) <- Map.toList filteredContext1]
          filteredArgs2 = [(a, t) | (a, (t, av)) <- Map.toList filteredContext2]

      -- Generate session types
      st1 <- stmtToST Map.empty filteredContext1 0 stmt1 
      st2 <- stmtToST Map.empty filteredContext2 0 stmt2
      case (st1, st2) of
        (Left e, _) -> die $ "Fehler in Block " ++ show i ++ "A:\n" ++ e
        (_, Left e) -> die $ "Fehler in Block " ++ show i ++ "B:\n" ++ e
        (Right (_, stA, _, _), Right (_, stB, _, _)) -> do
          putStrLn $ "\n===========================\n========== Paar " ++ show i ++ " =========\n===========================\n "

          case checkClosed stA of
            Left err -> putStrLn $ "Fehler in Block " ++ show i ++ "A: " ++ err
            Right () -> return ()
          case checkClosed stB of
            Left err -> putStrLn $ "Fehler in Block " ++ show i ++ "B: " ++ err
            Right () -> return ()
          putStrLn ("main context: " ++ show args')
          putStrLn "\nST A:"
          putStrLn (prettyPrintST stA)
          putStrLn "\nST B:"
          putStrLn (prettyPrintST stB)

          putStrLn "\nTests:"
          resEquiv <- testEquivalence' filteredContext1 filteredContext2 [] (addSkip stA) (addSkip stB)
          resDual <- testDuality filteredContext1 filteredContext2 [] (initialZ (initialZ Map.empty filteredArgs1 filteredContext1) filteredArgs2 filteredContext2) (addSkip stA) (addSkip stB)
          putStrLn ("A ~ B? " ++  show resEquiv)
          putStrLn ("A ^ B? " ++ show resDual)