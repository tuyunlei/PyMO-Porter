{-# LANGUAGE FlexibleInstances #-}
module PyMO where

import Data.Word8 ( Word8 )
import Data.Maybe ( fromMaybe, isJust, catMaybes, mapMaybe )
import Data.String (IsString (fromString))
import System.FilePath ( takeBaseName, (</>), (<.>) )
import Data.Char (isSpace, isDigit)
import Data.Bits (Bits(xor))
import Data.List (elemIndex)
import Prelude hiding (lines)
import qualified Prelude (lines)
import System.IO (openFile, IOMode (ReadMode), utf8, hSetEncoding, hGetContents)

type PosX = Float
type PosY = Float
type Pos = (PosX, PosY)
data Color = Color Word8 Word8 Word8 deriving (Show, Eq)
type Size = Int
type ShowImmediately = Bool
type CharaId = Int
data CharaClsId = CharaClsId CharaId | CharaClsA deriving (Show, Eq)
type Filename = String
data Transition = BGNofade | BGAlpha | BGFade | BGMask Filename deriving (Show, Eq)
type Layer = Int
type Time = Int
type IsLoop = Bool
type Var = String
data Op = EQ | NE | GE | GT | LE | LT deriving (Show, Eq)
data CoordMode
  = CoordMode0
  | CoordMode1
  | CoordMode2
  | CoordMode3
  | CoordMode4
  | CoordMode5
  | CoordMode6
  deriving (Show, Eq)

data Instr
  = Say (Maybe String) String
  | Text String Pos Pos Color Size ShowImmediately
  | TextOff
  | WaitKey
  | Title String
  | TitleDsp
  | Chara [(CharaId, Maybe Filename, PosX, Layer)] Time
  | CharaCls CharaClsId Time
  | CharaPos CharaId Pos CoordMode
  | Bg Filename Transition Time (Maybe Pos)
  | Flash Color Time
  | Quake
  | FadeOut Color Time
  | FadeIn Time
  | Movie Filename
  | Textbox Filename Filename
  | CharaQuake [CharaId]
  | CharaDown [CharaId]
  | CharaUp [CharaId]
  | Scroll Filename Pos Pos Time
  | CharaY CoordMode [(CharaId, Maybe Filename, Pos, Layer)] Time
  | CharaScroll CoordMode CharaId Filename Pos Pos Float Layer Time
  | AnimeOn Int Filename Pos Time IsLoop
  | AnimeOff Filename
  | CharaAnime CharaId Time Int [Pos]
  | Set Var (Either Var Int)
  | Add Var (Either Var Int)
  | Sub Var (Either Var Int)
  | Label String
  | Goto String
  | IfGoto Var Op (Either Var Int) String
  | Change String
  | Call String
  | Ret
  | Sel [String] (Maybe Filename)
  | SelectText [String] Pos Pos Color Int
  | SelectVar [(String, Var)] Pos Pos Color Int
  | SelectImg Filename [(Pos, Var)] Int
  | SelectImgs [(Filename, Pos, Var)] Instr
  | Wait Time
  | WaitSe
  | Rand Var Int Int
  | Bgm Filename IsLoop
  | BgmStop
  | Se Filename IsLoop
  | SeStop
  | Vo Filename
  | Load (Maybe Int)
  | Album (Maybe Filename)
  | Music
  | Date Filename Pos Color
  | Config
  deriving (Show, Eq)

data NameAlign = NameAlignLeft | NameAlignMiddle | NameAlignRight
  deriving (Show, Eq)

data Platform = PyGame | S60v3 | S60v5
  deriving (Show, Eq)

data ScriptType = PyMO | MO1 | MO2 deriving (Show, Eq)

instance IsString Platform where
  fromString "pygame" = PyGame
  fromString "s60v3" = S60v3
  fromString "s60v5" = S60v5
  fromString _ = error "Unknown platform"

instance IsString ScriptType where
  fromString "pymo" = PyMO
  fromString "mo1" = MO1
  fromString "mo2" = MO2
  fromString _ = error "Unknown script type"

instance IsString NameAlign where
  fromString "left" = NameAlignLeft
  fromString "middle" = NameAlignMiddle
  fromString "right" = NameAlignRight
  fromString _ = error "Unknown name align"

instance (Read a, Read b) => IsString (a, b) where
  fromString s = (read left, read $ tail right)
    where (left, right) = span (/= ',') s

instance IsString Color where
  fromString ['#',r',r'',g',g'',b',b''] =
    Color (read r)
          (read g)
          (read b)
    where r = "0x" ++ [r', r'']
          g = "0x" ++ [g', g'']
          b = "0x" ++ [b', b'']
  fromString intCol =
    Color (fromIntegral $ colInt `div` 256 `div` 256)
          (fromIntegral $ colInt `div` 256 `mod` 256)
          (fromIntegral $ colInt `mod` 256)
    where colInt = read intCol :: Int

instance IsString Transition where
  fromString "BG_NOFADE" = BGNofade
  fromString "BG_ALPHA" = BGAlpha
  fromString "BG_FADE" = BGFade
  fromString s = BGMask s

instance IsString CharaClsId where
  fromString "a" = CharaClsA
  fromString x = CharaClsId $ read x

instance IsString CoordMode where
  fromString "0" = CoordMode0
  fromString "1" = CoordMode1
  fromString "2" = CoordMode2
  fromString "3" = CoordMode3
  fromString "4" = CoordMode4
  fromString "5" = CoordMode5
  fromString "6" = CoordMode6
  fromString _ = error "Unknown coord mode"

data GameConfig = GameConfig {
  gametitle :: String,
  platform :: Platform,
  engineversion :: String,
  scripttype :: String,
  bgformat :: String,
  charaformat :: String,
  charamaskformat :: String,
  bgmformat :: String,
  seformat :: String,
  voiceformat :: String,
  font :: String,
  fontsize :: Int,
  fontaa :: Bool,
  hint :: Bool,
  prefetching :: Bool,
  grayselected :: Bool,
  playvideo :: Bool,
  textspeed :: Int,
  bgmvolume :: Int,
  vovolume :: Int,
  imagesize :: (Int, Int),
  startscript :: String,
  nameboxorig :: Pos,
  cgprefix :: String,
  textcolor :: Color,
  msgtb :: Pos,
  msglr :: Pos,
  namealign :: NameAlign
} deriving (Show, Eq)

data PyMOScript = PyMOScript Filename [Instr] deriving (Show, Eq)

type ForcOpened = Bool
type CGName = String
data CGAlbum = CGAlbum Filename [(Int, CGName, [Filename], ForcOpened)] deriving (Show, Eq)
newtype MusicGallery = MusicGallery [(Filename, String)] deriving (Show, Eq)

data PyMOGame =
  PyMOGame FilePath GameConfig [PyMOScript] [CGAlbum] (Maybe MusicGallery)
  deriving (Show, Eq)

-- split string by comma
splitArgs :: String -> [String]
splitArgs [] = []
splitArgs x = case elemIndex ',' x of
  Nothing -> [x]
  (Just p) -> first:splitArgs remain
              where first = take p x
                    remain = drop (p+1) x

-- load pymogame from pymogame directory
loadPyMOGame :: FilePath -> IO PyMOGame
loadPyMOGame path = do
  let getAlbumName (Album (Just x)) = Just x
      getAlbumName (Album Nothing) = Just "album_list"
      getAlbumName _ = Nothing
  let getInstrs (PyMOScript _ instrs) = instrs
  let isMusic Music = True
      isMusic _ = False

  config <- loadPyMOGameConfig $ path </> "gameconfig.txt"
  let scriptDir = path </> "script"
  scripts <- loadAllPyMOScripts scriptDir (startscript config)
  let allInstrs = concatMap getInstrs scripts
  let albumNames = mapMaybe getAlbumName allInstrs
  albums <- mapM (loadCGAlbum . (\name -> scriptDir </> name <.> "txt")) albumNames
  let hasMusicGallery = any isMusic allInstrs
  let getGallery = loadBGMGallery $ scriptDir </> "music_list.txt"
  musicGallery <- sequence $ if hasMusicGallery then Just getGallery else Nothing
  return $ PyMOGame path config scripts albums musicGallery

-- expand pymo package (.pak file) in given pymogame directory
-- if has no pak file, do nothing
-- https://github.com/Strrationalism/CPyMO/blob/main/cpymo/cpymo_package.c
expandPyMOPackage :: PyMOGame -> IO ()
expandPyMOPackage = undefined -- TODO

lines :: String -> [String]
lines = Prelude.lines . filter (/= '\r')

instance IsString GameConfig where
  fromString text =
    GameConfig {
      gametitle = get "" "gametitle",
      platform = fromString $ get "pygame" "platform",
      engineversion = get "pygame" "engineversion",
      scripttype = fromString $ get "pymo" "scripttype",
      bgformat = get ".png" "bgformat",
      charaformat = get ".png" "charaformat",
      charamaskformat = get ".png" "charamaskformat",
      bgmformat = get ".mp3" "bgmformat",
      seformat = get ".mp3" "seformat",
      voiceformat = get ".mp3" "voiceformat",
      font = get "-1" "font",
      fontsize = read $ get "16" "fontsize",
      fontaa = (> 0) $ read $ get "0" "fontaa",
      hint = (> 0) $ read $ get "1" "hint",
      prefetching = (> 0) $ read $ get "1" "prefetching",
      grayselected = (> 0) $ read $ get "1" "grayselected",
      playvideo = (> 0) $ read $ get "1" "playvideo",
      textspeed = read $ get "3" "textspeed",
      bgmvolume = read $ get "3" "bgmvolume",
      vovolume = read $ get "3" "vovolume",
      imagesize = fromString $ get "800,600" "imagesize",
      startscript = get "" "startscript",
      nameboxorig = fromString $ get "0,0" "nameboxorig",
      cgprefix = get "" "cgprefix",
      textcolor = fromString $ get "#FFFFFF" "textcolor",
      msgtb = fromString $ get "0,0" "msgtb",
      msglr = fromString $ get "0,0" "msglr",
      namealign = fromString $ get "left" "namealign"
    }
    where get def key = fromMaybe def $ lookup key kvs
          kvs = (\x -> (takeWhile (/= ',') x, tail $ dropWhile (/= ',') x)) <$> lines text

loadPyMOGameConfig :: FilePath -> IO GameConfig
loadPyMOGameConfig path = fromString <$> readFile path

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

readVarOrInt :: String -> Either Var Int
readVarOrInt x = if all isDigit x then Right $ read x else Left x

readCharaFilename :: String -> Maybe String
readCharaFilename "NULL" = Nothing
readCharaFilename x = Just x

getBgTime :: String -> Time
getBgTime s
  | s == "BG_VERYFAST" = 300
  | otherwise = read s

getSeIsLoop :: String -> IsLoop
getSeIsLoop "1" = True
getSeIsLoop "0" = False
getSeIsLoop s = read s

mkPyMOInstr :: String -> [String] -> Maybe Instr
mkPyMOInstr "say" [text] = Just $ Say Nothing text
mkPyMOInstr "say" [ch, text] = Just $ Say (Just ch) text
mkPyMOInstr "text" [content, x1, y1, x2, y2, color, size, showImm] =
  Just $ Text content (read x1, read y1) (read x2, read y2)
                      (fromString color) (read size) ((> 0) $ read showImm)
mkPyMOInstr "text_off" [] = Just TextOff
mkPyMOInstr "waitkey" [] = Just WaitKey
mkPyMOInstr "title" [title] = Just $ Title title
mkPyMOInstr "title_dsp" [] = Just TitleDsp
mkPyMOInstr "chara" x = 
  Just $ Chara (readCharas x) (read $ last x)
  where 
    readCharas (charaId:filename:pos:layer:next) = 
      (read charaId, readCharaFilename filename, read pos, read layer):readCharas next
    readCharas _ = []
mkPyMOInstr "chara_cls" [charaID] = Just $ CharaCls (fromString charaID) 300
mkPyMOInstr "chara_cls" [charaID,time] = Just $ CharaCls (fromString charaID) (read time)
mkPyMOInstr "chara_pos" [charaID,new_x,new_y, coord_mode] =
  Just $ CharaPos (read charaID) (read new_x, read new_y) (fromString coord_mode)
mkPyMOInstr "bg" [filename] = Just $ Bg filename BGAlpha 300 Nothing
mkPyMOInstr "bg" [filename, transition, time] =
  Just $ Bg filename (fromString transition) (getBgTime time) Nothing
mkPyMOInstr "bg" [filename, transition, time, x, y] =
  Just $ Bg filename (fromString transition) (read time) (Just (read x, read y))
mkPyMOInstr "flash" [color, time] = Just $ Flash (fromString color) (read time)
mkPyMOInstr "quake" [] = Just Quake
mkPyMOInstr "fade_out" [color, time] = Just $ FadeOut (fromString color) (read time)
mkPyMOInstr "fade_in" [time] = Just $ FadeIn (read time)
mkPyMOInstr "movie" [filename] = Just $ Movie filename
mkPyMOInstr "textbox" [message, name] = Just $ Textbox message name
mkPyMOInstr "chara_quake" ids = Just $ CharaQuake (map read ids)
mkPyMOInstr "chara_down" ids = Just $ CharaDown (map read ids)
mkPyMOInstr "chara_up" ids = Just $ CharaUp (map read ids)
mkPyMOInstr "scroll" [filename, startx, starty, endx, endy, time] =
  Just $ Scroll filename (read startx, read starty) (read endx, read endy) (read time)
mkPyMOInstr "chara_y" (coordMode:x) = 
  Just $ CharaY (fromString coordMode) (readCharaYs x) (read $ last x)
  where
    readCharaYs (charaId:filename:x:y:layer:n) =
      (read charaId, readCharaFilename filename, (read x, read y), read layer):readCharaYs n
    readCharaYs _ = []
mkPyMOInstr "chara_scroll" [coord_mode, charaId, filename, sx, sy, ex, ey, ba, layer, time] =
  Just $ CharaScroll (fromString coord_mode) 
                     (read charaId)
                     filename 
                     (read sx, read sy) 
                     (read ex, read ey) 
                     (read ba) 
                     (read layer) 
                     (read time)
mkPyMOInstr "anime_on" [num, filename, x ,y, interval, isloop] =
  Just $ AnimeOn (read num) filename (read x, read y) (read interval) (read isloop)
mkPyMOInstr "anime_off" [filename] = Just $ AnimeOff filename
mkPyMOInstr "chara_anime" (charaId:period:loopNum:offsets) =
  Just $ CharaAnime (read charaId) (read period) (read loopNum) (readOffsets offsets)
  where readOffsets [] = []
        readOffsets [_] = []
        readOffsets (x:y:xs) = (read x, read y) : readOffsets xs
mkPyMOInstr "set" [name, value] = Just $ Set name $ readVarOrInt value
mkPyMOInstr "add" [name, value] = Just $ Add name $ readVarOrInt value
mkPyMOInstr "sub" [name, value] = Just $ Sub name $ readVarOrInt value
mkPyMOInstr "label" [name] = Just $ Label name
mkPyMOInstr "goto" [name] = Just $ Goto name
-- mkPyMOInstr "if...goto"
mkPyMOInstr "change" [filename] = Just $ Change filename
mkPyMOInstr "call" [filename] = Just $ Call filename
mkPyMOInstr "ret" [] = Just Ret
-- mkPyMOInstr "sel"
mkPyMOInstr "select_text" (choiceNum:x) =
  Just $ SelectText (take (read choiceNum) x) pos1 pos2 col initpos
  where readOpts [x1,y1,x2,y2,col,initpos] = 
          ((read x1, read y1), (read x2, read y2), fromString col, read initpos)
        readOpts _ = error "select_text: invalid options"
        (pos1, pos2, col, initpos) = readOpts $ drop (read choiceNum) x
-- mkPyMOInstr "select_var"
-- mkPyMOInstr "select_img"
-- mkPyMOInstr "select_imgs"
mkPyMOInstr "wait" [time] = Just $ Wait (read time)
mkPyMOInstr "wait_se" [] = Just WaitSe
mkPyMOInstr "rand" [name, min, max] = Just $ Rand name (read min) (read max)
mkPyMOInstr "bgm" [filename] = Just $ Bgm filename True
mkPyMOInstr "bgm" [filename, isloop] = Just $ Bgm filename (read isloop)
mkPyMOInstr "bgm_stop" [] = Just BgmStop
mkPyMOInstr "se" [filename] = Just $ Se filename True
mkPyMOInstr "se" [filename, isloop] = Just $ Se filename (getSeIsLoop isloop)
mkPyMOInstr "se_stop" [] = Just SeStop
mkPyMOInstr "vo" [filename] = Just $ Vo filename
mkPyMOInstr "load" [] = Just $ Load Nothing
mkPyMOInstr "load" [save_num] = Just $ Load $ Just (read save_num)
mkPyMOInstr "album" [] = Just $ Album Nothing
mkPyMOInstr "album" [filename] = Just $ Album $ Just filename
mkPyMOInstr "music" [] = Just Music
mkPyMOInstr "date" [bg, x, y, color] = Just $ Date bg (read x, read y) (fromString color)
mkPyMOInstr "config" [] = Just Config

-- More Instrs
mkPyMOInstr _ _ = Nothing

parseLineToPyMOInstr :: String -> Maybe Instr
parseLineToPyMOInstr ('#':x) =
  mkPyMOInstr command args
  where (command, args) = (takeWhile (/= ' ') x, splitArgs $ trim $ dropWhile (/= ' ') x)
parseLineToPyMOInstr _ = Nothing

removeComment :: String -> String
removeComment = takeWhile (/= ';')

loadPyMOScript :: FilePath -> IO PyMOScript
loadPyMOScript path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  l <- lines <$> hGetContents h 
  let instrs = mapMaybe (parseLineToPyMOInstr . trim . removeComment) l
  return $ PyMOScript (takeBaseName path) instrs

-- load scripts from 'change' 'call' command.
loadAllPyMOScripts :: FilePath -> String -> IO [PyMOScript]
loadAllPyMOScripts scriptDir startupScript = load [startupScript] []
  where load :: [String] -> [String] -> IO [PyMOScript]
        load [] scriptLoaded = return []
        load scriptToLoad scriptLoaded = do
            let needToLoad :: Instr -> Maybe String
                needToLoad (Change filename) = Just filename
                needToLoad (Call filename) = Just filename
                needToLoad _ = Nothing
            let getNeedToLoadList :: PyMOScript -> [String]
                getNeedToLoadList (PyMOScript _ instrs) =
                    filter (`notElem` scriptLoaded) (mapMaybe needToLoad instrs)
            let getScriptPath :: String -> FilePath
                getScriptPath name = scriptDir </> name <.> "txt"

            scripts <- mapM (loadPyMOScript . getScriptPath) scriptToLoad
            let needToLoads = concatMap getNeedToLoadList scripts
            newScripts <- load needToLoads (scriptLoaded ++ scriptToLoad)
            return $ scripts ++ newScripts

loadCGAlbum :: FilePath -> IO CGAlbum
loadCGAlbum path = do
  let parseArgsToCGItem :: [String] -> Maybe (Int, CGName, [Filename], ForcOpened)
      parseArgsToCGItem (p:n:cg:xs) = do
        Just (read p, cg, names, forcOpened)
        where num = read n :: Int
              names = take num xs
              remains = drop num xs
              forcOpened = not (null remains) && read (last remains) == 1
      parseArgsToCGItem _ = Nothing

  content <- readFile path
  let items = mapMaybe (parseArgsToCGItem . splitArgs) (lines content)
  return $ CGAlbum (takeBaseName path) items

instance IsString MusicGallery where
  fromString = MusicGallery . mapMaybe (parseLine . trim) . lines
    where parseLine [] = Nothing
          parseLine x = do
            p <- elemIndex ',' x
            let file = take p x
                name = drop (p + 1) x
            return (file, name)


loadBGMGallery :: FilePath -> IO MusicGallery
loadBGMGallery path = fromString <$> readFile path

class HasPyMOVars a where
  getPyMOVars :: a -> [Var]

instance HasPyMOVars Instr where
  getPyMOVars (Set v _) = [v]
  getPyMOVars (Add v _) = [v]
  getPyMOVars (Sub v _) = [v]
  getPyMOVars (IfGoto v _ (Right _) _) = [v]
  getPyMOVars (IfGoto v _ (Left v') _) = [v, v']
  getPyMOVars (Sel {}) = ["FSEL"]
  getPyMOVars (SelectText {}) = ["FSEL"]
  getPyMOVars (SelectVar {}) = ["FSEL"]
  getPyMOVars (SelectImg {}) = ["FSEL"]
  getPyMOVars (SelectImgs {}) = ["FSEL"]
  getPyMOVars (Rand v _ _) = [v]
  getPyMOVars (Date {}) = ["FMONTH", "FDATE"]
  getPyMOVars _ = []

instance HasPyMOVars a => HasPyMOVars [a] where
  getPyMOVars = concatMap getPyMOVars

instance HasPyMOVars PyMOScript where
  getPyMOVars (PyMOScript _ x) = getPyMOVars x

instance HasPyMOVars PyMOGame where
  getPyMOVars (PyMOGame _ _ x _ _) = getPyMOVars x

