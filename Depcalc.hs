module Depcalc where

import Control.Applicative((<$>))
import Control.Monad
import Data.Char(isSpace)
import Data.Graph
import Data.List(intersperse,sort,nub)
import System.Environment
import System.FilePath.Posix
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

x |> f = f x

portify info = info |> DT.lines |> (toPort <$>)
  where
    toPort line = line |> DT.split (== ':') |> parse
    parse [package, deps] =
      ( DT.strip $ package
      , filter (not . DT.null) $ DT.words deps
      )

generateGraph ports = map toNode ports |> graphFromEdges
  where toNode (name, deps) = (name, name, deps)

translate (name,_,_) = name
forward = id
backward = transposeG

depends graph port fromVertex toVertex tr
  = vertexed |> reachable graph |> map (tr . fromVertex) |> sort
  where
    vertexed = case (toVertex port) of
      Just id -> id
      Nothing -> error $ (DT.unpack port) ++ " cannot be found"

printLines = putStrLn . DT.unpack . DT.unlines

--
-- Calculate dependencies between ports based on a "describe file".  This
-- file contains information on which package depends the given package
-- (see hs-dependencies.sh).
--
-- The result is a list of all dependants according to the direction of
-- search: "forward" or "backward" (inverted).
--
calculateDependency describe dir port = do
  contents <- DTI.readFile describe
  let (h,f,g) = contents |> portify |> generateGraph
  return $ depends (dir h) port f g translate

showDependency describe dir port =
  calculateDependency describe dir port >>= printLines

showDependencyL describe dir = do
  liftM (nub . sort . concat) . mapM (calculateDependency describe dir)

--
-- Calculate a topology for ports -- it is an ordering between graph
-- elements.  It is ideal for determining the order of how ports should
-- be added/updated when committing them.
--
-- The result is a list ordered by their dependencies.
--
calculateTopology describe dir port = do
  contents <- DTI.readFile describe
  let (h,f,g) = contents |> portify |> generateGraph
  let (h1,f1,g1) = graphFromEdges $ depends (dir h) port f g id
  return $ topSort h1 |> map (translate . f1) |> reverse

showTopology describe dir port =
  calculateTopology describe dir port >>= printLines

hackageMk = "lang/ghc/bsd.hackage.mk"

inTree portsdir = do
  let pid = DT.pack "_port="
  contents <- fmap (map DT.strip . DT.lines) $ DTI.readFile (portsdir </> hackageMk)
  return $ map (DT.unpack . fst . DT.breakOn pid) $ filter (pid `DT.isInfixOf`) contents

notInTree portsdir ports = do
  intree <- inTree portsdir
  return $ filter (`notElem` intree) ports
