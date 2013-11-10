module Main where

import qualified Data.Text as T
import           MakeMP3Copies
import           Shelly
default (T.Text)

fromRoot :: Shelly.FilePath
fromRoot = fromText $ T.pack "."

main = shelly $ verbosely $ do
    processDir fromRoot
