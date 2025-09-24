{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import Text.Megaparsec
import Parser
import Datastructure

main :: IO ()
main = do
    let input0 = "var c1 chan int\nvar c2 chan int\nvar c chan int\nc1 ::= make (chan int)\nc2 ::= make (chan int)\nif b then { c = c1 } else { c = c2 }"
    case runParser parseProgram "" input0 of
      Left err -> putStrLn $ errorBundlePretty err 
      Right (Program decs stmt) -> do
        putStrLn $ "Parsed Statement:\n" ++ input0
        putStrLn ("Session Type: " ++ stmtToST stmt ++ "\n")
        putStrLn ("Abstract Value Context: " ++ show (inferContext decs stmt))      
        putStrLn ("Dual ST: " ++ stmtToST (dual stmt))  
        putStrLn ("Variables: " ++ show decs ++ "\n")
    let input1 = "var c1 chan int\nvar c2 chan int\nvar x int\nif x > 0 then c1 <- 2*x else c2 <- 2*x\nclose c1\nclose c2"
    case runParser parseProgram "" input1 of
      Left err -> putStrLn $ errorBundlePretty err
      Right (Program decs stmt)  -> do
        putStrLn $ "Parsed Statement:\n" ++ input1
        putStrLn ("Session Type: " ++ stmtToST stmt ++ "\n")
        putStrLn ("Abstract Value Context: " ++ show (inferContext decs stmt))
        putStrLn ("Dual ST: " ++ stmtToST (dual stmt))  
        putStrLn ("Variables: " ++ show decs ++ "\n")
    let input2 = "var c chan int\nx = <- c \nif (2 > 0) then c <- 2*x else skip \nclose c"
    case runParser parseProgram "" input2 of
      Left err -> putStrLn $ errorBundlePretty err
      Right (Program decs stmt)  -> do
        putStrLn $ "Parsed Statement:\n" ++ input2
        putStrLn ("Session Type: " ++ stmtToST stmt ++ "\n")
        putStrLn ("Abstract Value Context: " ++ show (inferContext decs stmt))
        putStrLn ("Dual ST: " ++ stmtToST (dual stmt))  
        putStrLn ("Variables: " ++ show decs ++ "\n")
    let input3 = "var c chan int\nfor (i=0;i<10;i++) { c <- i }\nclose c"
    case runParser parseProgram "" input3 of
      Left err -> putStrLn $ errorBundlePretty err
      Right (Program decs stmt)  -> do
        putStrLn $ "Parsed Statement:\n" ++ input3
        putStrLn ("Session Type: " ++ stmtToST' (inferContext decs stmt) stmt ++ "\n")
        putStrLn ("Abstract Value Context: " ++ show (inferContext decs stmt))
        putStrLn ("Dual ST: " ++ stmtToST (dual stmt))  
        putStrLn ("Variables: " ++ show decs ++ "\n")
    let input4 = "var c chan int\nx = <- c\nif x > 0 then c <- 2*x else\nif x < 0 then c <- 3*x else skip\nclose c"
    case runParser parseProgram "" input4 of
      Left err -> putStrLn $ errorBundlePretty err
      Right (Program decs stmt)  -> do
        putStrLn $ "Parsed Statement:\n" ++ input4
        putStrLn ("Session Type: " ++ stmtToST' (inferContext decs stmt) stmt ++ "\n")
        putStrLn ("Abstract Value Context: " ++ show (inferContext decs stmt))
        putStrLn ("Dual ST: " ++ stmtToST (dual stmt))  
        putStrLn ("Variables: " ++ show decs ++ "\n")
    let input5 = "var c1 chan int\nvar c2 chan int\nfor (i=0;i<5;i++) {\nx = <- c1\nc2 <- x*2\ny = <- c1}\nclose c1\nclose c2"
    case runParser parseProgram "" input5 of
      Left err -> putStrLn $ errorBundlePretty err
      Right (Program decs stmt)  -> do
        putStrLn $ "Parsed Statement:\n" ++ input5
        putStrLn ("Session Type: " ++ stmtToST' (inferContext decs stmt) stmt ++ "\n")
        putStrLn ("Abstract Value Context: " ++ show (inferContext decs stmt))
        putStrLn ("Dual ST: " ++ stmtToST (dual stmt))  
        putStrLn ("Variables: " ++ show decs ++ "\n")
    let input6 = "var c1 chan int\nvar c2 chan int\nx = <- c1\ny = <- c2\nif ( x > 0 ) then {c1 <- 2*x\nc2 <- 2*x} else\nif x < 0 then {c1 <- 3*x\nc2 <- 3*x} else skip\nclose c1\nclose c2"
    case runParser parseProgram "" input6 of
      Left err -> putStrLn $ errorBundlePretty err
      Right (Program decs stmt)  -> do
        putStrLn $ "Parsed Statement:\n" ++ input6
        putStrLn ("Session Type: " ++ stmtToST' (inferContext decs stmt) stmt ++ "\n")
        putStrLn ("Abstract Value Context: " ++ show (inferContext decs stmt))
        putStrLn ("Dual ST: " ++ stmtToST (dual stmt))  
        putStrLn ("Variables: " ++ show decs ++ "\n")
    let input7 = "var i int\nvar x int\nvar y int\nvar c chan int\ni := 0\nx := i + 1\ny := x\nif (y>0) then c <- 2*x else skip"
    case runParser parseProgram "" input7 of
      Left err -> putStrLn $ errorBundlePretty err 
      Right (Program decs stmt) -> do 
        putStrLn $ "Parsed Statement:\n" ++ input7
        putStrLn ("Session Type: " ++ stmtToST' (inferContext decs stmt) stmt ++ "\n")
        putStrLn ("Abstract Value Context: " ++ show (inferContext decs stmt))
        putStrLn ("Dual ST: " ++ stmtToST (dual stmt))  
        putStrLn ("Variables: " ++ show decs ++ "\n")       
