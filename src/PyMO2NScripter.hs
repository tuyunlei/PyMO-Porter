{-# LANGUAGE ExistentialQuantification #-}

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
import Data.Bifunctor (Bifunctor(bimap))
import GHC.IO.Handle (hGetEncoding, hPutStr)
import System.IO (openFile, hSetEncoding, IOMode (WriteMode), hClose, openBinaryFile)
import System.FilePath ((</>))

-- expand pymo pak file and generate nscript.dat (or 0.txt for nscripter?)
-- https://kaisernet.fka.cx/onscripter/api/NScrAPI-framed.html
pymo2nscripter :: (FilePath -> String -> IO ()) -> PyMOGame -> IO ()
pymo2nscripter write game = do
  let (nscript, log) = getNScriptAndLog $ writePyMOGame game
      (PyMOGame path _ _ _ _) = game
  putStrLn log
  write (path </> "0.txt") nscript

pymo2nscripterUTF8 :: PyMOGame -> IO ()
pymo2nscripterUTF8 = pymo2nscripter writeFile

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
writePyMOGame (PyMOGame path gameconfig scripts album musicGallery) = do
  writeGameConfig gameconfig

writePyMOScript :: PyMOScript -> NSWriter ()
writePyMOScript _ = pure () -- TODO

getNScriptAndLog :: NSWriter () -> (String, String)
getNScriptAndLog w = bimap unlines unlines r
  where r = execWriter w

