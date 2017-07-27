{-
Created       : 2013 Sep 09 (Mon) 17:41:15 by carr.
Last Modified : 2017 Jul 26 (Wed) 17:42:13 by Harold Carr.
-}

module Main where

import qualified Data.Text     as T
import           MakeMP3Copies
import           Shelly

fromRoot :: Shelly.FilePath
fromRoot = fromText $ T.pack "."

main :: IO ()
main = shelly $ verbosely $
    processDir fromRoot

-- End of file.
