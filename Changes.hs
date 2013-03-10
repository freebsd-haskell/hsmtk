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
  let (deleted,added,misc) = findChanges orig new
  let [numDeleted,numAdded] = fmap length [deleted,added]
  when (numAdded > 0) (do
    printf "New ports (%d):\n\n" numAdded
    forM_ added $ \[p,v] -> do
      let [p',v'] = map DT.unpack [p,v]
      printf "%-40s %s\n" p' v')
  let (bumped,misc') = partition isBumped misc
  let (updated,_) = partition isUpdated misc'
  let [numBumped,numUpdated] = fmap length [bumped,updated]
  when (numBumped > 0) (do
    printf "\n\n"
    printf "Bumped ports (%d):\n\n" numBumped
    forM_ bumped $ \(p,v1,v2) -> do
      let [p',v1',v2'] = map DT.unpack [p,v1,v2]
      printf "%-40s %-16s --> %s\n" p' v1' v2')
  when (numUpdated > 0) (do
    printf "\n\n"
    printf "Updated ports (%d):\n\n" numUpdated
    forM_ updated $ \(p,v1,v2) -> do
      let [p',v1',v2'] = map DT.unpack [p,v1,v2]
      printf "%-40s %-16s --> %s\n" p' v1' v2')
  when (numDeleted > 0) (do
    printf "\n\n"
    printf "Removed ports (%d):\n\n" numDeleted
    forM_ deleted $ \[p,v] -> do
      let [p',v'] = map DT.unpack [p,v]
      printf "%-40s %s\n" p' v')

findChanges l1@((x@[nx,vx]):xs) l2@((y@[ny,vy]):ys)
  | nx == ny  = (mempty,mempty,[(nx,vx,vy)]) `mappend` findChanges xs ys
  | nx < ny   = ([x],mempty,mempty) `mappend` findChanges xs l2
  | nx > ny   = (mempty,[y],mempty) `mappend` findChanges l1 ys

findChanges [] ys = (mempty,ys,mempty)
findChanges xs [] = (xs,mempty,mempty)

isBumped (_,v1,v2) =
  case (map (DT.splitOn "_") [v1,v2]) of
    [[ver1],[ver2,_]]     -> ver1 == ver2
    [[ver1,r1],[ver2,r2]] -> ver1 == ver2 && r1 /= r2
    _ -> False

isUpdated (_,v1,v2) = v1 /= v2
