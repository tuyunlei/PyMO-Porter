module PyMO where

import Data.Word8

type PosX = Float
type PosY = Float
type Pos = (PosX, PosY)
type Color = (Word8, Word8, Word8)
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
  | Set Var Int
  | Add Var Int
  | Sub Var Int
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
  | Date
  | Config
  deriving (Show, Eq)

data NameAlign = NameAlignLeft | NameAlignCenter | NameAlignRight
  deriving (Show, Eq)

data GameConfig = GameConfig {
  gametitle :: String,
  platform :: String,
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

data PyMOScript = PyMOScript Filename [Instr]

data PyMOGame = PyMOGame FilePath GameConfig [PyMOScript]

-- load pymogame from pymogame directory
loadPyMOGame :: FilePath -> IO PyMOGame
loadPyMOGame = undefined

-- expand pymo package (.pak file) in given pymogame directory
-- if has no pak file, do nothing
-- call shell command or executable `cpymo-tool unpack` to expand pak file
expandPyMOPackage :: PyMOGame -> IO ()
expandPyMOPackage = undefined
