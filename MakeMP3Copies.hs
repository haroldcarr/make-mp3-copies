
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MakeMP3Copies where

import           Control.Applicative
import           Control.Exception (bracket, handle, SomeException)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Shelly
import           System.Directory
import           System.FilePath
import           System.IO (IOMode(..), hClose, hFileSize, openFile)
default (T.Text)

-- TODO: get this from Main
toRoot   = "/tmp/JUNK/"

processDir fromPath = do
    contents <- ls fromPath
    forM_ contents $ \from -> do
            isDir <- test_d from
            if isDir
                then do { maybeCreateDir from; processDir from}
                else processFile from $ takeExtension $ fpToString from

maybeCreateDir from = do
    let to = mkToFilePath from
    dirExists <- test_d to
    if dirExists
        then                  say "DIR EXISTS"  to
        else do { mkdir_p to; say "DIR CREATED" to }

processFile from ".flac" = maybeDo convert True  False from
processFile from ".mp3"  = maybeDo copy    False True  from
processFile from ".jpg"  = maybeDo copy    False True  from
processFile from _       = say "IGNORED" from

maybeDo f extP sizeP from = do
    let to = mkToFilePath $ if extP then (fromText (T.pack (replaceExtension (fpToString from) ".mp3"))) else from
    fileExists <- test_f to
    if fileExists
        then doIf f sizeP from to
        else      f       from to

doIf f sizeP from to = do
    fromSize <- lio getFileSize         from
    fromTime <- lio getModificationTime from
    toSize   <- lio getFileSize         to
    toTime   <- lio getModificationTime to
    if fromTime > toTime || (sizeP && (fromJust fromSize) /= (fromJust toSize))
        then f from to
        else say "FILE EXISTS" to

convert from to = do
    flacToMp3 (toTextIgnore from) (toTextIgnore to)
    say "FILE CONVERTED" to
  where
    flacToMp3 from to = run_ "ffmpeg" ["-i", from, "-qscale:a", "0", to]

copy from to = do
    cp from to
    say "FILE COPIED" to

mkToFilePath path =
    (fpToString toRoot) Shelly.</> (fpToString path)

fpToString fp = T.unpack $ toTextIgnore fp

say msg fp =
    liftIO $ putStrLn $ show (fpToString fp) ++ " " ++ msg

lio f fp =
    liftIO . f $ fpToString fp

-- from Real World Haskell
getFileSize path = handle handler $
    bracket (openFile path ReadMode) (hClose) (\h -> do
        size <- hFileSize h
        return $ Just size)
  where
    handler :: SomeException -> IO (Maybe Integer)
    handler _ = return Nothing

-- End of file.
