{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import Text.Megaparsec
import Parser
import Datastructure

-- checks if a Session Type is valid (atm only if all channels close)
checkChannelBehavior :: ChannelBehavior -> Either String ()
checkChannelBehavior (ChannelBehavior []) = Right ()
checkChannelBehavior (ChannelBehavior ((c, s):xs)) = if checkClose s then checkChannelBehavior (ChannelBehavior xs) else Left ("Kanal" ++ show c ++ " wird nicht geschlossen.")

-- Checks if a Session Type closes
checkClose :: Statement -> Bool
checkClose st = case st of
  Sequence _ s2 -> checkClose s2
  End _         -> True
  _             -> False


main :: IO ()
main = do
    let input1 = "c <- 42\nclose c"
    case runParser parseStatement "" input1 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input1
        let cb = assignChannelsToStmts s
        putStrLn $ "ChannelBehavior:" ++ show cb
        case checkChannelBehavior cb of
         Left err -> putStrLn $ "Ungültig: " ++ err ++ "\n"
         Right () -> putStrLn $ "Gültig\n"
    let input2 = "x = <- c \nif x > 0 then c <- 2*x else skip \nclose c"
    case runParser parseStatement "" input2 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input2
        let cb = assignChannelsToStmts s
        putStrLn $ "ChannelBehavior:\n" ++ show cb
        case checkChannelBehavior cb of
         Left err -> putStrLn $ "Ungültig: " ++ err ++ "\n"
         Right () -> putStrLn $ "Gültig\n"
    let input3 = "i := 0\nfor i < 10 { c <- i }\nclose c"
    case runParser parseStatement "" input3 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input3
        let cb = assignChannelsToStmts s
        putStrLn $ "ChannelBehavior:\n" ++ show cb
        case checkChannelBehavior cb of
         Left err -> putStrLn $ "Ungültig: " ++ err ++ "\n"
         Right () -> putStrLn $ "Gültig\n"
    let input4 = "x = <- c\nif x > 0 then c <- 2*x else\nif x < 0 then c <- 3*x else skip\nclose c"
    case runParser parseStatement "" input4 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input4
        let cb = assignChannelsToStmts s
        putStrLn $ "ChannelBehavior:" ++ show cb
        case checkChannelBehavior cb of
         Left err -> putStrLn $ "Ungültig: " ++ err ++ "\n"
         Right () -> putStrLn $ "Gültig\n"
    let input5 = "for i<5 {\nx = <- c1\nc2 <- x*2\n}\nclose c1\nclose c2"
    case runParser parseStatement "" input5 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input5
        let cb = assignChannelsToStmts s
        putStrLn $ "ChannelBehavior:" ++ show cb
        case checkChannelBehavior cb of
         Left err -> putStrLn $ "Ungültig: " ++ err ++ "\n"
         Right () -> putStrLn $ "Gültig\n"
    let input6 = "x = <- c1\ny = <- c2\nif x > 0 then c1 <- 2*x\nc2 <- 2*x else\nif x < 0 then c1 <- 3*x\nc2 <- 3*x else skip\nclose c1\nclose c2"
    case runParser parseStatement "" input6 of
      Left err -> putStrLn $ errorBundlePretty err
      Right s  -> do
        putStrLn $ "Parsed Statement:\n" ++ input6
        let cb = assignChannelsToStmts s
        putStrLn $ "ChannelBehavior:" ++ show cb
        case checkChannelBehavior cb of
         Left err -> putStrLn $ "Ungültig: " ++ err ++ "\n"
         Right () -> putStrLn $ "Gültig\n"