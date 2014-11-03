{-
Created       : 2013 Sep 09 (Mon) 17:41:15 by carr.
Last Modified : 2014 Nov 02 (Sun) 18:01:42 by Harold Carr.
-}

module Main where

import qualified Data.Text     as T
import           MakeMP3Copies
import           Shelly
default (T.Text)

fromRoot :: Shelly.FilePath
fromRoot = fromText $ T.pack "."

main :: IO ()
main = shelly $ verbosely $
    processDir fromRoot

-- End of file.
