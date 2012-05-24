
import Control.Applicative((<$>))
import Control.Monad
import Data.Char(isSpace)
import Data.ByteString.Lazy(readFile)
import Data.ByteString.Lazy.Char8(concat,singleton,split,init,last,unpack,lines)
import Data.Graph
import Data.List(intersperse,sort,nub)
import System.Environment
import Prelude hiding (readFile,concat,init,last,lines)
import qualified Prelude as P
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as T

x |> f = f x

dropWS = reverse . dropWhile isSpace . reverse . dropWhile isSpace

portify info
  = info |> lines |> (toPort <$>)
  where
    toPort line = line |> split ':' |> parse
    parse [package, deps] = (dropWS $ unpack package, filter (not . null) $ unpack <$> split ' ' deps)

generateGraph ports
  = map toNode ports |> graphFromEdges
  where toNode (name, deps) = (name, name, deps)

translate (name,_,_) = name
forward = id
backward = transposeG

depends graph port fromVertex toVertex tr
  = vertexed |> reachable graph |> map (tr . fromVertex) |> sort
  where
    vertexed = case (toVertex port) of
      Just id -> id
      Nothing -> error $ port ++ " cannot be found"

printLines = putStrLn . unlines

--
-- Calculate dependencies between ports based on a "describe file".  This
-- file contains information on which package depends the given package
-- (see hs-dependencies.sh).
--
-- The result is a list of all dependants according to the direction of
-- search: "forward" or "backward" (inverted).
--
calculateDependency describe dir port = do
  contents <- readFile describe
  let (h,f,g) = contents |> portify |> generateGraph
  return $ depends (dir h) port f g translate

showDependency describe dir port =
  calculateDependency describe dir port >>= printLines

showDependencyL describe dir = do
  liftM (nub . sort . P.concat) . mapM (calculateDependency describe dir)

--
-- Calculate a topology for ports -- it is an ordering between graph
-- elements.  It is ideal for determining the order of how ports should
-- be added/updated when committing them.
--
-- The result is a list ordered by their dependencies.
--
calculateTopology describe dir port = do
  contents <- readFile describe
  let (h,f,g) = contents |> portify |> generateGraph
  let (h1,f1,g1) = graphFromEdges $ depends (dir h) port f g id
  return $ topSort h1 |> map (translate . f1) |> reverse

showTopology describe dir port =
  calculateTopology describe dir port >>= printLines

hackageMk = "lang/ghc/bsd.hackage.mk"

inTree portsdir = do
  let pid = T.pack "_port="
  contents <- fmap (P.map T.strip . T.lines) $ T.readFile (portsdir </> hackageMk)
  return $ P.map (T.unpack . P.fst . T.breakOn pid) $ P.filter (pid `T.isInfixOf`) contents

notInTree portsdir ports = do
  intree <- inTree portsdir
  return $ filter (`notElem` intree) ports
