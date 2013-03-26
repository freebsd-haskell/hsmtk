{-# LANGUAGE OverloadedStrings #-}
-- "Find differences between package versions."
import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Environment
import Text.Printf
import Depcalc

main = do
  progname <- getProgName
  args <- getArgs
  case (length args) of
    2 -> printChanges Nothing (args !! 0) (args !! 1)
    3 -> printChanges (Just $ args !! 2) (args !! 0) (args !! 1)
    _ -> putStrLn $ usage progname

usage = printf "USAGE: %s pkgversions pkgversions [dependencies]"

getList = liftM (sort . map (DT.splitOn " ") . DT.lines) . DTI.readFile

data Changes = Changes
  { chNumAdded   :: Int
  , chNumDeleted :: Int
  , chNumBumped  :: Int
  , chNumUpdated :: Int
  , chAdded      :: [[String]]
  , chDeleted    :: [[String]]
  , chBumped     :: [[String]]
  , chUpdated    :: [[String]]
  }

printChanges z x y = computeChanges z x y >>= displayChanges

unpack = map (map DT.unpack)

normalize x = x'
  where (_:x':_) = DT.splitOn "/hs-" x

getEffect fp (p:_) = calculateDependency fp backward (normalize p)

resolve :: [DT.Text] -> DT.Text -> DT.Text
resolve xs p =
  case (find (s `DT.isInfixOf`) xs) of
    Just y -> y
    _      -> error "This should not happen."
  where s = "/hs-" `DT.append` p

computeChanges deps x y = do
  orig <- getList x
  new  <- getList y
  let (deleted,added,misc)  = findChanges orig new
  let [numDeleted,numAdded] = length <$> [deleted,added]
  (bumped,numBumped,updated,numUpdated) <-
    case deps of
      Just fp -> do
        let (b',misc') = partition isBumped misc
        let (u,_)      = partition isUpdated misc'
        affected <- (\\)
          <$> (nub . concat <$> mapM (getEffect fp) u)
          <*> pure (normalize . head <$> u)
        let ub = sort (resolve (head <$> new) <$> affected)
        let b = bump <$> filter ((`elem` ub) . head) orig
        let [nb,nu]   = length <$> [b,u]
        return (b,nb,u,nu)
      _ -> do
        let (b,misc') = partition isBumped misc
        let (u,_)     = partition isUpdated misc'
        let [nb,nu]   = length <$> [b,u]
        return (b,nb,u,nu)
  return $ Changes
    numAdded numDeleted numBumped numUpdated
    (unpack added) (unpack deleted)
    (unpack bumped) (unpack updated)

displayChanges :: Changes -> IO ()
displayChanges c = do
  let numAdded = chNumAdded c
  let added = chAdded c
  when (numAdded > 0) (do
    printf "New ports (%d):\n\n" numAdded
    forM_ added $ \[p,v] -> do
      printf "%-40s %s\n" p v)
  let numBumped = chNumBumped c
  let bumped = chBumped c
  when (numBumped > 0) (do
    printf "\n\n"
    printf "Bumped ports (%d):\n\n" numBumped
    forM_ bumped $ \[p,v1,v2] -> do
      printf "%-40s %-16s --> %s\n" p v1 v2)
  let numUpdated = chNumUpdated c
  let updated = chUpdated c
  when (numUpdated > 0) (do
    printf "\n\n"
    printf "Updated ports (%d):\n\n" numUpdated
    forM_ updated $ \[p,v1,v2] -> do
      printf "%-40s %-16s --> %s\n" p v1 v2)
  let numDeleted = chNumDeleted c
  let deleted = chDeleted c
  when (numDeleted > 0) (do
    printf "\n\n"
    printf "Removed ports (%d):\n\n" numDeleted
    forM_ deleted $ \[p,v] -> do
      printf "%-40s %s\n" p v)

findChanges l1@((x@[nx,vx]):xs) l2@((y@[ny,vy]):ys)
  | nx == ny  = (mempty,mempty,[[nx,vx,vy]]) `mappend` findChanges xs ys
  | nx < ny   = ([x],mempty,mempty) `mappend` findChanges xs l2
  | nx > ny   = (mempty,[y],mempty) `mappend` findChanges l1 ys

findChanges [] ys = (mempty,ys,mempty)
findChanges xs [] = (xs,mempty,mempty)

isBumped [_,v1,v2] =
  case (map (DT.splitOn "_") [v1,v2]) of
    [[ver1],[ver2,_]]     -> ver1 == ver2
    [[ver1,r1],[ver2,r2]] -> ver1 == ver2 && r1 /= r2
    _ -> False

isUpdated [_,v1,v2] = v1 /= v2

bump [p,v1] =
  case ((DT.splitOn "_") v1) of
    [_]        -> [p,v1,v1 `DT.append` "_1"]
    [ver,rev]  -> [p,v1,DT.concat [ver,"_",rev']]
      where rev' = DT.pack $ show (((read $ DT.unpack rev)) + 1)
