
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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

startingPath = "."
copyRoot = "/tmp/JUNK/"

main = shelly $ verbosely $ do
    processDir startingPath

processDir :: Shelly.FilePath -> Sh ()
processDir path = do
    contents <- ls path
    forM_ contents $ \fp -> do
            isDir <- test_d fp
            if isDir
                then do { maybeCreateDir fp; processDir fp}
                else processFile fp $ takeExtension $ fpToString fp

maybeCreateDir :: Shelly.FilePath -> Sh ()
maybeCreateDir fp = do
    let cp = mkCpFilePath fp
    dirExists <- test_d cp
    if dirExists
        then                  say "DIR EXISTS"  cp
        else do { mkdir_p cp; say "DIR CREATED" cp }

processFile :: Shelly.FilePath -> String -> Sh ()
processFile fp ".flac" = maybeConvert  fp
processFile fp ".mp3"  = maybeCopy     fp
processFile fp ".jpg"  = maybeCopy     fp
processFile fp _       = say "IGNORED" fp

maybeConvert fp = do
    let cp = mkCpFilePath (fromText (T.pack (replaceExtension (fpToString fp) ".mp3")))
    fileExists <- test_f cp
    if fileExists
        then doIf convert False fp cp
        else      convert       fp cp

maybeCopy fp = do
    let cp = mkCpFilePath fp
    fileExists <- test_f cp
    if fileExists
        then doIf copy True fp cp
        else      copy      fp cp

doIf :: (Shelly.FilePath -> Shelly.FilePath -> Sh ())
        -> Bool -> Shelly.FilePath -> Shelly.FilePath -> Sh ()
doIf f sizeP fp cp = do
    fpSize <- liftIO . getFileSize         $ fpToString fp
    fpTime <- liftIO . getModificationTime $ fpToString fp
    cpSize <- liftIO . getFileSize         $ fpToString cp
    cpTime <- liftIO . getModificationTime $ fpToString cp
    if fpTime > cpTime || (sizeP && (fromJust fpSize) /= (fromJust cpSize))
        then f fp cp
        else say "FILE EXISTS" cp

convert fp copy = do
    flacToMp3 (toTextIgnore fp) (toTextIgnore copy)
    say "FILE CONVERTED" copy
  where
    flacToMp3 from to = run_ "ffmpeg" ["-i", from, "-qscale:a", "0", to]

copy fp copyFile = do
    cp fp copyFile
    say "FILE COPIED" copyFile

mkCpFilePath :: Shelly.FilePath -> Shelly.FilePath
mkCpFilePath path =
    (fpToString copyRoot) Shelly.</> (fpToString path)

fpToString :: Shelly.FilePath -> String
fpToString fp = T.unpack $ toTextIgnore fp

say msg fp =
    liftIO $ putStrLn $ show (fpToString fp) ++ " " ++ msg

getFileSize :: System.FilePath.FilePath -> IO (Maybe Integer)
getFileSize path = handle handler $
    bracket (openFile path ReadMode) (hClose) (\h -> do
        size <- hFileSize h
        return $ Just size)
  where
    handler :: SomeException -> IO (Maybe Integer)
    handler _ = return Nothing

-- End of file.
