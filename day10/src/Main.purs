module Main where

import Prelude

import Data.Array (catMaybes, delete, filter, head, sort, take, (:))
import Data.Foldable (maximum)
import Data.Int.Parse (parseInt, toRadix)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

joltagePath :: Int -> Int -> Array Int -> Maybe (Array Int)
joltagePath j maxJ as | j == maxJ - 3 = Just [3]
                      | j >  maxJ - 3 = Nothing
                      | otherwise     = head $ do
                        a <- filter (\a -> a - j <= 3) $ take 3 as
                        case joltagePath a maxJ (delete a as) of
                          Just ds -> [(a - j) : ds]
                          Nothing -> []

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "resources/input.txt"
  let joltages = sort $ catMaybes $ map (flip parseInt $ toRadix 10) $ split (Pattern "\n") input
  case maximum joltages of
    Just m -> do
      log $ show $ joltagePath 0 (m + 3) joltages
    Nothing -> do
      log "No joltages!"
