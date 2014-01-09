{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Directory
import System.FilePath.Posix
import System.IO
import Text.Printf

hasString :: DT.Text -> DT.Text -> Bool
hasString str = not . null . filter (str `DT.isInfixOf`) . DT.lines

withLines :: ([DT.Text] -> [DT.Text]) -> DT.Text -> DT.Text
withLines f = DT.unlines . f . DT.lines

bumpRevision :: DT.Text -> DT.Text
bumpRevision input
  | hasString "PORTREVISION" input = withLines (map f) input
  | hasString "PORTVERSION" input  = withLines insertRevision input
  | otherwise = input
  where
    f line
      | "PORTREVISION" `DT.isInfixOf` line = DT.concat [pre, value', post]
      | otherwise                           = line
      where
        (pre,line')  = DT.break (`elem` ['0'..'9']) line
        (value,post) = DT.break (`elem` "\t ") line'
        value'
          | not (DT.null value) =
            DT.pack $ show ((read (DT.unpack value) :: Integer) + 1)
          | otherwise = value

    insertRevision xs = ys1 ++ [y, revisionLine] ++ ys2
      where
        (ys1,y:ys2)   = break (hasString "PORTVERSION") xs
        revisionLine  = "PORTREVISION=\t1"

changeMakefile :: (DT.Text -> DT.Text) -> FilePath -> IO ()
changeMakefile f path = do
  let makefile = path </> "Makefile"
  exist <- doesFileExist makefile
  if exist
    then DTI.readFile makefile >>= DTI.writeFile makefile . f
    else hPutStrLn stderr $ printf "Warning: %s cannot be found." makefile

on :: (DT.Text -> DT.Text) -> [FilePath] -> IO ()
action `on` ports = forM_ ports (changeMakefile action)
