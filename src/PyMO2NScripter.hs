module PyMO2NScripter
  ( pymo2nscripterGB18030
  , pymo2nscripterUTF8
  , pymo2nscripterSJIS) where

import PyMO
import GHC.IO.Encoding

-- expand pymo pak file and generate nscript.dat (or 0.txt for nscripter?)
-- https://kaisernet.fka.cx/onscripter/api/NScrAPI-framed.html
pymo2nscripter :: TextEncoding -> PyMOGame -> IO ()
pymo2nscripter = undefined

pymo2nscripterGB18030 :: PyMOGame -> IO ()
pymo2nscripterGB18030 game = mkTextEncoding "CP54936" >>= \x -> pymo2nscripter x game

pymo2nscripterUTF8 :: PyMOGame -> IO ()
pymo2nscripterUTF8 game = mkTextEncoding "UTF-8" >>= \x -> pymo2nscripter x game

pymo2nscripterSJIS :: PyMOGame -> IO ()
pymo2nscripterSJIS game = mkTextEncoding "CP932" >>= \x -> pymo2nscripter x game
