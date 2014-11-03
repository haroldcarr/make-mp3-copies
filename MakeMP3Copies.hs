{-
Created       : 2013 Sep 09 (Mon) 17:41:15 by carr.
Last Modified : 2014 Nov 02 (Sun) 17:25:05 by Harold Carr.
-}

{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MakeMP3Copies where

import           Control.Exception      (SomeException, bracket, handle)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe             (fromJust)
import           Data.String
import qualified Data.Text              as T
import           Shelly
import           System.Directory
import           System.FilePath
import           System.IO              (IOMode (..), hClose, hFileSize,
                                         openFile)
default (T.Text)

-- TODO: get this from Main
toRoot :: Data.String.IsString a => a
toRoot   = "/tmp/JUNK/"

processDir :: Shelly.FilePath -> Sh ()
processDir fromPath = do
    contents <- ls fromPath
    forM_ contents $ \from -> do
            isDir <- test_d from
            if isDir
                then do { maybeCreateDir from; processDir from}
                else processFile from $ takeExtension $ fpToString from

maybeCreateDir :: Shelly.FilePath -> Sh ()
maybeCreateDir from = do
    let to = mkToFilePath from
    dirExists <- test_d to
    if dirExists
        then                  say "DIR EXISTS"  to
        else do { mkdir_p to; say "DIR CREATED" to }

processFile :: (Eq a, Data.String.IsString a) => Shelly.FilePath -> a -> Sh ()
processFile from ".flac" = maybeDo convert True  False from
processFile from ".mp3"  = maybeDo copy    False True  from
processFile from ".jpg"  = maybeDo copy    False True  from
processFile from _       = say "IGNORED" from

maybeDo :: (Shelly.FilePath -> Shelly.FilePath -> Sh ())
            -> Bool -> Bool -> Shelly.FilePath -> Sh ()
maybeDo f extP sizeP from = do
    let to = mkToFilePath $ if extP then (fromText (T.pack (replaceExtension (fpToString from) ".mp3"))) else from
    fileExists <- test_f to
    if fileExists
        then doIf f sizeP from to
        else      f       from to

doIf :: Control.Monad.IO.Class.MonadIO m =>
               (Shelly.FilePath -> Shelly.FilePath -> m ())
     -> Bool -> Shelly.FilePath -> Shelly.FilePath -> m ()
doIf f sizeP from to = do
    fromSize <- lio getFileSize         from
    fromTime <- lio getModificationTime from
    toSize   <- lio getFileSize         to
    toTime   <- lio getModificationTime to
    if fromTime > toTime || (sizeP && (fromJust fromSize) /= (fromJust toSize))
        then f from to
        else say "FILE EXISTS" to

convert :: Shelly.FilePath -> Shelly.FilePath -> Sh ()
convert from0 to0 = do
    flacToMp3 (toTextIgnore from0) (toTextIgnore to0)
    say "FILE CONVERTED" to0
  where
    flacToMp3 from to = run_ "ffmpeg" ["-i", from, "-qscale:a", "9", to]

copy :: Shelly.FilePath -> Shelly.FilePath -> Sh ()
copy from to = do
    cp from to
    say "FILE COPIED" to

mkToFilePath :: Shelly.FilePath -> Shelly.FilePath
mkToFilePath path0 =
    (fpToString toRoot) Shelly.</> (fpToString path0)

fpToString :: Shelly.FilePath -> String
fpToString fp = T.unpack $ toTextIgnore fp

say :: Control.Monad.IO.Class.MonadIO m =>
       [Char] -> Shelly.FilePath -> m ()
say msg fp =
    liftIO $ putStrLn $ show (fpToString fp) ++ " " ++ msg

lio :: Control.Monad.IO.Class.MonadIO m =>
       (String -> IO a) -> Shelly.FilePath -> m a
lio f fp =
    liftIO . f $ fpToString fp

-- from Real World Haskell
getFileSize :: Prelude.FilePath -> IO (Maybe Integer)
getFileSize path0 = handle handler $
    bracket (openFile path0 ReadMode) (hClose) (\h -> do
        size <- hFileSize h
        return $ Just size)
  where
    handler :: SomeException -> IO (Maybe Integer)
    handler _ = return Nothing

-- End of file.
