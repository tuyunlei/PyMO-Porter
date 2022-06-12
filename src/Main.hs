module Main where

import PyMO
import PyMO2NScripter

targets :: [(String, PyMOGame -> IO (), String)]
targets =
  [ ("ns-gb", pymo2nscripterGB18030, "NScripter/ONScripter in GB18030"),
    ("ns-utf8", pymo2nscripterUTF8, "ONScripter-EN"),
    ("ns-sjis", pymo2nscripterSJIS, "NScripter/ONScripter in SJIS")
  ]

main :: IO ()
main = do  
  putStrLn "Hello"
