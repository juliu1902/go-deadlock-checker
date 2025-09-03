{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import Text.Megaparsec
import Parser
import Datastructure

main :: IO ()
main = do
    let input1 = "c <- 42\nclose c"
    case runParser parseStatement "" input1 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input1
        let cb = assignChannelsToStmts s
        putStrLn $ "ChannelBehavior:\n" ++ show cb
    let input2 = "x = <- c \nif x > 0 then c <- 2*x else skip \nclose c"
    case runParser parseStatement "" input2 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input2
        let cb = assignChannelsToStmts s
        putStrLn $ "ChannelBehavior:\n" ++ show cb
    let input3 = "i := 0\nfor i < 10 { c <- i }\nclose c"
    case runParser parseStatement "" input3 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input3
        let cb = assignChannelsToStmts s
        putStrLn $ "ChannelBehavior:\n" ++ show cb
