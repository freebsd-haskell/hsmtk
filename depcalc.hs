--
-- Simple inverted port dependency calculation
--
-- It tells you which ports are affected by changing a given port.
-- The result always includes the port itself.
--
-- Requires a "DESCRIBE" file which can be generated from the ports tree
-- by issuing `make describe | grep -v "^===>" > DESCRIBE` in the root.
--

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC8
import qualified Data.Map as DM
import qualified Data.Graph as DG
import qualified Data.List as DL
import System.Environment

type Name      = BS.ByteString
type BuildDeps = BS.ByteString
type Port      = (Name, BuildDeps)
type Ports     = [Port]
type Deps      = DM.Map String [String]
type PortIds   = DM.Map String Int
type PortDis   = DM.Map Int String
type PortGraph = DG.Graph

convertPath :: BS.ByteString -> BS.ByteString -> BS.ByteString
convertPath category path
  = ( BSC8.concat
    . DL.intersperse (BSC8.singleton '/')
    . (:) (norm cat)
    . take 1
    . nsr
    ) path
  where
    normalize s
      = if ((BSC8.last s) == '/')
        then BSC8.init s
        else s
    norm cat
      = if (BSC8.unpack cat == "..")
        then category
        else cat
    nsr = reverse . BSC8.split '/' . normalize
    cat = (head . drop 1 . take 2 . nsr) path

ports :: BS.ByteString -> Ports
ports index
  = map (\line -> parse $ BSC8.split '|' line) $ BSC8.lines index
  where
    parse (package : path : prefix : comment : pkgdescr : maintainer :
      categories : _ : _ : _ : bdepends : rdepends : www : _) = (name, bdepends)
        where
          name     = convertPath category path
          category = head $ BSC8.split ' ' categories

getAllBuildDependencies :: Ports -> [(String,[String])]
getAllBuildDependencies ports
  = map conversion ports
  where
    conversion (pname,pbds) = (name, deps)
      where
        name     = BSC8.unpack pname
        deps     = map (BSC8.unpack . convertPath category) $ BSC8.split ' ' pbds
        category = head $ BSC8.split '/' pname

buildNodes :: [(String,[String])] -> [(String,String,[String])]
buildNodes ports = map translation ports
  where translation (n, ds) = (n, n, ds)

generateGraph :: Ports -> (PortGraph, DG.Vertex -> (String,String,[String]), String -> Maybe DG.Vertex)
generateGraph ports = DG.graphFromEdges $ buildNodes deps
  where deps = getAllBuildDependencies ports

depends :: PortGraph -> String -> (DG.Vertex -> (String,String,[String])) -> (String -> Maybe DG.Vertex) -> [String]
depends graph port f g = DL.sort $ map translate $ DG.reachable graph v
  where
    v = case (g port) of
          Just id -> id
          Nothing -> error $ "Port \"" ++ port ++ "\" cannot be found (broken INDEX or mistyped name?)"

    translate v = case (f v) of
          (n,_,_) -> n

main = do
  args <- getArgs
  prog <- getProgName
  if ((length args) < 2)
    then putStrLn $ unlines
      ["Simple inverted port dependency calculation"
      ,""
      ,"It tells you which ports are affected by changing a given port.  The result"
      ,"always includes the port itself."
      ,""
      ,"USAGE: " ++ prog ++ " DESCRIBE category/port"
      ,""
      ,""
      ,"DESCRIBE files can be constructed by the following command:"
      ,""
      ,"$ make -C ${PORTSDIR} describe | grep -v \"^===>\" > DESCRIBE"]
    else do
      let describe = args !! 0
      let port = args !! 1
      contents <- BS.readFile describe
      let (g,f,v) = generateGraph $ ports contents
      let graph = DG.transposeG g
      let output = unlines $ depends graph port f v
      putStrLn output