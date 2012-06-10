
import Prelude as P
import Data.List
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as T

hackageMk = "lang/ghc/bsd.hackage.mk"

hsPorts portsdir = do
  contents <- fmap (P.map T.strip . T.lines) $ T.readFile (portsdir </> hackageMk)
  let ports = P.filter (pid `T.isInfixOf`) contents
  return $ P.map (T.unpack . getDirectory) ports
  where
    getDirectory = (!! 1) . T.words . snd . T.breakOn pid
    pid          = T.pack "_port="

printHsPorts portsdir =
  hsPorts portsdir >>= putStrLn . unlines . sort
