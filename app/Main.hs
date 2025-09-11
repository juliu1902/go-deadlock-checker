{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import Text.Megaparsec
import Parser
import Datastructure

main :: IO ()
main = do
    let input1 = "var c1 chan int\nvar c2 chan int\nvar x int\nc1 ::= make (chan int)\nc2 ::= make (chan int)\nif x > 0 then c1 <- 2*x else c2 <- 2*x\nclose c1\nclose c2"
    case runParser parseProgram "" input1 of
      Left err -> putStrLn $ errorBundlePretty err
      Right (Program decs stmt)  -> do
        putStrLn $ "Parsed Statement:\n" ++ input1
        putStrLn ("Session Type: " ++ stmtToST stmt ++ "\n")
        putStrLn ("Variables: " ++ show decs ++ "\n")
    let input2 = "c ::= make (chan int)\nx = <- c \nif 2 > 0 then c <- 2*x else skip \nclose c"
    case runParser parseStatement "" input2 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input2
        putStrLn ("Session Type: " ++ stmtToST s++ "\n")
    let input3 = "c ::= make (chan int)\ni := 0\nfor i < 10 { c <- i }\nclose c"
    case runParser parseStatement "" input3 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input3
        putStrLn ("Session Type: " ++ stmtToST s ++ "\n")
    let input4 = "c ::= make (chan int)\nx = <- c\nif x > 0 then c <- 2*x else\nif x < 0 then c <- 3*x else skip\nclose c"
    case runParser parseStatement "" input4 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input4
        putStrLn ("Session Type: " ++ stmtToST s++ "\n")
    let input5 = "c1 ::= make (chan int)\nc2 ::= make (chan int)\nfor i<5 {\nx = <- c1\nc2 <- x*2\ny = <- c1}\nclose c1\nclose c2"
    case runParser parseStatement "" input5 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input5
        putStrLn ("Session Type: " ++ stmtToST s++ "\n")
    let input6 = "c1 ::= make (chan int)\nc2 ::= make (chan int)\nx = <- c1\ny = <- c2\nif x > 0 then {c1 <- 2*x\nc2 <- 2*x} else\nif x < 0 then {c1 <- 3*x\nc2 <- 3*x} else skip\nclose c1\nclose c2"
    case runParser parseStatement "" input6 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input6
        putStrLn ("Session Type: " ++ stmtToST s++ "\n")