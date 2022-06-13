module PyMO2NScripter
{-  ( pymo2nscripterGB18030
  , pymo2nscripterUTF8
  , pymo2nscripterSJIS)  -}
where

import PyMO
import GHC.IO.Encoding
import GHC.Base (undefined)
import Control.Monad.Writer
import Prelude hiding (log)

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

type NSWriter = Writer ([String], [String])

ns :: String -> NSWriter ()
ns x = tell ([x], [])

log :: String -> NSWriter ()
log x = tell ([], [x])

wrapStr :: String -> String
wrapStr x = "\"" ++ x ++ "\""

writeGameConfig :: GameConfig -> NSWriter ()
writeGameConfig gc = do
  case imagesize gc of (640, 480) -> pure ()
                       (800, 600) -> ns ";mode800"
                       (400, 300) -> ns ";mode400"
                       (320, 240) -> ns ";mode320"
                       (w, h) -> log $ "Screen size " ++ show w ++ show h ++ " not supported."
  ns "*define"
  ns $ "caption " ++ wrapStr (gametitle gc)
  ns "game"

writePyMOGame :: PyMOGame -> NSWriter ()
writePyMOGame = undefined

writePyMOScript :: PyMOScript -> NSWriter ()
writePyMOScript = undefined

getNScript :: NSWriter () -> String
getNScript = unlines . fst . execWriter

getLog :: NSWriter a -> String
getLog = unlines . snd . execWriter
