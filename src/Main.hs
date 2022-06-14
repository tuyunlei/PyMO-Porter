module Main where

import PyMO
import PyMO2NScripter
import System.Environment (getArgs)

targets :: [(String, String)]
targets =
  [ ("ns-gb", "NScripter/ONScripter in GB18030"),
    ("ns-utf8", "ONScripter-EN"),
    ("ns-sjis", "NScripter/ONScripter in SJIS")
  ]

help :: IO ()
help = do  
  putStrLn "PyMO-Porter"
  putStrLn "Port pymo games to other platforms."
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "    PyMO-Porter <target> <pymogame-dir>"
  putStrLn ""
  putStrLn "Targets:"
  mapM_ (\(k, v) -> putStrLn $ "    " ++ k ++ "\t\t" ++ v) targets
  putStrLn ""

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [target, pymogame] -> do
      game <- loadPyMOGame pymogame
      case target of
        "ns-gb" -> pymo2nscripterGB18030 game
        "ns-utf8" -> pymo2nscripterUTF8 game
        "ns-sjis" -> pymo2nscripterSJIS game
        _ -> help
    _ -> help

