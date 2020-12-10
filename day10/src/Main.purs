module Main where

import Prelude

import Data.Array (catMaybes)
import Data.Int.Parse (parseInt, toRadix)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "resources/input.txt"
  let joltages = catMaybes $ map (flip parseInt $ toRadix 10) $ split (Pattern "\n") input
  log $ show joltages
