module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Node.Encoding (Encoding (..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "resources/input.txt"
  log input
