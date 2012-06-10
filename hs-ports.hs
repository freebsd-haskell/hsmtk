
import Prelude as P
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as T

portsDir  = "/usr/ports"
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

getPortsDir = do
  env <- getEnvironment
  return $ maybe portsDir id (lookup "PORTSDIR" env)

bailOut s = putStrLn $ "ERROR: " ++ s

-- A main function for running from a shell.
main = do
  args <- getArgs
  case args of
    ["ports"] -> do
      pdir <- getPortsDir
      mkThere <- doesFileExist (pdir </> hackageMk)
      if mkThere
        then printHsPorts pdir
        else bailOut "No bsd.hackage.mk found."
    _ -> bailOut "Nissing or invalid query."
