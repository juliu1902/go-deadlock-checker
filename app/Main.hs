{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import qualified Data.Map as Map (fromList)
import Text.Megaparsec
import Parser
import Datastructure
import System.Environment (getArgs)
import System.Exit (die)

-- Are s1 and s2 equivalent?
testEquiv :: Statement -> Statement -> Bool
testEquiv s1 s2 = testEquivalence (normalizeST s1) (normalizeST s2)

-- Is s1 dual to s2?
testDual :: Statement -> Statement -> Bool
testDual s1 s2 = testEquiv (dual s1) s2

-- Parses a program and returns its Session Type or an error if it couldn't be parsed
buildST :: String -> Either String (Statement, Context)
buildST src =
  case runParser parseProgram "" src of
    Left err -> Left (errorBundlePretty err)
    Right (Program decs stmt) ->
      let (st, ctxt) = stmtToST (freshInitialContext (initialContext decs)) stmt
      in Right (st, ctxt)

-- creates a list of strings out of a long string where --- stands for a split
splitAllByLine :: String -> [String]
splitAllByLine x = helper (lines x) where
  helper :: [String] -> [String]
  helper [] = [""]
  helper (l:ls) = if l == "---" then [""] ++ helper ls else case (helper ls) of
    (y:ys) -> [(unlines ([l] ++ lines y))] ++ ys
    _ -> [l]

-- make program blocks into pairs 
toPairs :: [a] -> Either String [(a,a)]
toPairs (a:b:rest) = do
  pairs <- toPairs rest
  return ((a,b):pairs)
toPairs [] = Right []
toPairs [_] = Left "Ungerade Anzahl von Programmblöcken: jeder Test braucht zwei Blöcke."

main :: IO ()
main = do
  args <- getArgs
  path <- case args of
            (p:_) -> pure p
            []    -> pure "inputs.txt"
  content <- readFile path
  let blocks = filter (not . all (`elem` "\n\t ")) (splitAllByLine content) -- spaces are ignored

  case toPairs blocks of
    Left e -> die e
    Right ps -> mapM_ (uncurry runCase) (zip [1..] ps)

runCase :: Int -> (String, String) -> IO ()
runCase i (srcA, srcB) =
  case (buildST srcA, buildST srcB) of
    (Left e, _) -> die $ "Fehler beim Parsen/Typisieren von Block " ++ show i ++ "A:\n" ++ e
    (_, Left e) -> die $ "Fehler beim Parsen/Typisieren von Block " ++ show i ++ "B:\n" ++ e
    (Right (stA, ctA), Right (stB, ctB)) -> do
      putStrLn $ "========== Paar " ++ show i ++ " =========="
      putStrLn   "ST A:"
      putStrLn (prettyPrintST stA)
      putStrLn $ "Context A: " ++ show ctA
      putStrLn   "\nST B:"
      putStrLn (prettyPrintST stB)
      putStrLn $ "Context B: " ++ show ctB


      putStrLn   "\nNormalformen:"
      putStrLn ("A: " ++ prettyPrintST (normalizeST stA))
      putStrLn ("B: " ++ prettyPrintST (normalizeST stB))

      putStrLn   "\nTests:"
      putStrLn ("A ~ B?        " ++ show (testEquiv stA stB))
      putStrLn ("A ~ dual(B)?  " ++ show (testDual stA stB))
      putStrLn ("dual(A) ~ B?  " ++ show (testDual stB stA))
      putStrLn ""
