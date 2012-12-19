{-# LANGUAGE OverloadedStrings #-}
-- "Find differences between package versions."
import Control.Monad
import Data.List
import Data.Monoid
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Environment
import Text.Printf

main = do
  progname <- getProgName
  args <- getArgs
  if (length args == 2)
    then printChanges (args !! 0) (args !! 1)
    else putStrLn (printf "USAGE: %s pkgversions pkgversions" progname)

getList = liftM (sort . map (DT.splitOn " ") . DT.lines) . DTI.readFile

printChanges x y = do
  orig <- getList x
  new <- getList y
  let (deleted,added,modified) = findChanges orig new
  printf "New ports (%d):\n\n" (length added)
  forM added $ \[p,v] -> do
    let [p',v'] = map DT.unpack [p,v]
    printf "%-40s %-16s\n" p' v'
  printf "\n\n"
  printf "Updated ports (%d):\n\n" (length modified)
  forM modified $ \(p,v1,v2) -> do
    let [p',v1',v2'] = map DT.unpack [p,v1,v2]
    printf "%-40s %-16s --> %-16s\n" p' v1' v2'
  printf "\n\n"
  printf "Removed ports (%d):\n\n" (length deleted)
  forM deleted $ \[p,v] -> do
    let [p',v'] = map DT.unpack [p,v]
    printf "%-40s %-16s\n" p' v'
  return ()

findChanges l1@((x@[nx,vx]):xs) l2@((y@[ny,vy]):ys)
  | nx == ny  = (mempty,mempty,[(nx,vx,vy)]) `mappend` findChanges xs ys
  | nx < ny   = ([x],mempty,mempty) `mappend` findChanges xs l2
  | nx > ny   = (mempty,[y],mempty) `mappend` findChanges l1 ys

findChanges [] ys = (mempty,ys,mempty)
findChanges xs [] = (xs,mempty,mempty)
